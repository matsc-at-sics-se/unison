{-|
Copyright   :  Copyright (c) 2016, RISE SICS AB
License     :  BSD3 (see the LICENSE file)
Maintainer  :  roberto.castaneda@ri.se
-}
{-
Main authors:
  Roberto Castaneda Lozano <roberto.castaneda@ri.se>

This file is part of Unison, see http://unison-code.github.io
-}
module Unison.Target.X86.Transforms
    (extractReturnRegs,
     handlePromotedOperands,
     liftReturnAddress,
     revertFixedFrame,
     myLowerFrameIndices,
     stackIndexReadsSP) where

import qualified Data.Map as M
import qualified Data.Set as S
import MachineIR
import Unison
import Unison.Analysis.FrameOffsets
import Unison.Target.X86.Common
import Unison.Target.X86.X86RegisterDecl
import Unison.Target.X86.SpecsGen.X86InstructionDecl
import Unison.Target.X86.Registers()

{-
    o12: [eax] <- (copy) [t4]
    o13: [] <- RETQ [eax]
    o18: [] <- (out) [...]
->
    o13: [] <- RETQ [42]
    o18: [] <- (out) [..., t4:eax]
-}

extractReturnRegs _ (
  SingleOperation {oOpr = Virtual VirtualCopy {
                      oVirtualCopyS = t,
                      oVirtualCopyD = Register ret}}
  :
  j @ SingleOperation {oOpr = Natural bj @ Branch {
                          oBranchIs = [TargetInstruction RETQ],
                          oBranchUs = [Register ret']}}
  :
  o @ SingleOperation {oOpr = Virtual
                               (Delimiter od @ Out {oOuts = outs})}
  :
  rest) _ | ret == ret' =
   (
    rest,
    [j {oOpr = Natural bj {oBranchUs = [mkBound (mkMachineImm 42)]}},
     o {oOpr = Virtual (Delimiter od {oOuts = outs ++
                                              [preAssign t (Register ret)]})}]
   )

{-
    o13: [] <- RETQ []
    o18: [] <- (out) [...]
->
    o13: [] <- RETQ [42]
    o18: [] <- (out) [...]
-}

extractReturnRegs _ (
  j @ SingleOperation {oOpr = Natural bj @ Branch {
                          oBranchIs = [TargetInstruction RETQ],
                          oBranchUs = []}}
  :
  rest) _ =
   (
    rest,
    [j {oOpr = Natural bj {oBranchUs = [mkBound (mkMachineImm 42)]}}]
   )

{-
    o13: [] <- TCRETURNdi64 [@spio_map_unix_error,0]
    o20: [] <- (out) [rsp,edi,esi]
->
    o13: [] <- TCRETURNdi64 [@spio_map_unix_error,0]
    o20: [] <- (out) []
-}

extractReturnRegs _ (
  j @ SingleOperation {oOpr = Natural (TailCall {})}
  :
  o @ SingleOperation {oOpr = Virtual (Delimiter od @ Out {})}
  :
  rest) _ =
   (
    rest,
    [j,
     o {oOpr = Virtual (Delimiter od {oOuts = []})}]
   )

extractReturnRegs _ (o : rest) _ = (rest, [o])

{-
    o10: [t14,t15] <- IMUL64m [%stack.0,1,_,8,_,t13] (mem: 1)
->
    o10: [t14:rax,t15:rdx] <- IMUL64m [%stack.0,1,_,8,_,t13:rax] (mem: 1)
-}

handlePromotedOperands
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = [TargetInstruction i],
                                 oDs = defs, oUs = uses})} =
  let ws    = filter (writesSideEffect i) promotedRegs
      rs    = filter (readsSideEffect i) promotedRegs
      defs' = preAssignOperands defs ws
      uses' = preAssignOperands uses rs
  in o {oOpr = Natural ni {oDs = defs', oUs = uses'}}
handlePromotedOperands o = o

preAssignOperands ops regs =
  let regOps = map (\r -> Register (TargetRegister r)) regs
      no = length ops
      nr = length regOps
      (original, promoted) = splitAt (no - nr) ops
      promoted' = map (\(t, r) -> preAssign t r) (zip promoted regOps)
  in original ++ promoted'

-- This transform creates a fixed frame object to represent the return address
-- which is implicit in LLVM's input.

liftReturnAddress f @ Function {fFixedStackFrame = fobjs} =
  let ra     = mkFrameObject (newFrameIndex fobjs) (-8) (Just 8) 8 Nothing
      fobjs' = fobjs ++ [ra]
  in f {fFixedStackFrame = fobjs'}

-- This reverts the fixed stack frame so that both the fixed and the variable
-- stack frame grow upwards. This is expected by the 'liftFixedFrameObjects',
-- 'computeFrameOffsets', and 'lowerFrameSize' passes in 'uni export'.

revertFixedFrame f @ Function {fFixedStackFrame = fobjs} =
  f {fFixedStackFrame = map revertDirection fobjs}

-- This transform replaces stack frame object indices in the code by actual
-- RSP + immediates.  It essentially overrules lowerFrameIndices.

myLowerFrameIndices f @ Function {fCode = code, fFixedStackFrame = fobjs,
                                  fStackFrame = objs} =
  let occ = slotSet (fobjs ++ objs)
      fsize    = if S.null occ then 0 else - S.findMin occ
      code'    = replaceFIsByImms fsize True fobjs code
      code''   = replaceFIsByImms fsize False objs code'
  in f {fCode = code''}

replaceFIsByImms fsize fixed objs code =
  let idxToOff = M.fromList [(foIndex fo, (foOffset fo) + fsize) | fo <- objs]
  in mapToOperationInBlocks
     (liftFIif fixed idxToOff) code

liftFIif fixed idxToOff (Bundle os) =
  mkBundle $ map (liftFIif fixed idxToOff) os

liftFIif fixed idxToOff
  o @ SingleOperation {oOpr = Natural ni @ (Linear {oUs = [(Bound (MachineFrameIndex idx fixed' off1)),
                                                           use2,
                                                           use3,
                                                           (Bound (MachineImm off2)),
                                                           use5]})}
  | fixed == fixed'
  = let use1' = mkRegister (mkTargetRegister RSP)
        use4' = mkBound (mkMachineImm $ (idxToOff M.! idx) + off1 + off2)
    in
    o {oOpr = Natural ni {oUs = [use1',use2,use3,use4',use5]}}
liftFIif fixed idxToOff
  o @ SingleOperation {oOpr = Natural ni @ (Linear {oUs = [(Bound (MachineFrameIndex idx fixed' off1)),
                                                           use2,
                                                           use3,
                                                           (Bound (MachineImm off2)),
                                                           use5,
                                                           use6]})}
  | fixed == fixed'
  = let use1' = mkRegister (mkTargetRegister RSP)
        use4' = mkBound (mkMachineImm $ (idxToOff M.! idx) + off1 + off2)
    in
    o {oOpr = Natural ni {oUs = [use1',use2,use3,use4',use5,use6]}}
liftFIif fixed idxToOff
  o @ SingleOperation {oOpr = Natural ni @ (Linear {oUs = [use1,
                                                           (Bound (MachineFrameIndex idx fixed' off1)),
                                                           use3,
                                                           use4,
                                                           (Bound (MachineImm off2)),
                                                           use6]})}
  | fixed == fixed'
  = let use2' = mkRegister (mkTargetRegister RSP)
        use5' = mkBound (mkMachineImm $ (idxToOff M.! idx) + off1 + off2)
    in
    o {oOpr = Natural ni {oUs = [use1,use2',use3,use4,use5',use6]}}
liftFIif _ _ o = o

stackIndexReadsSP
  o @ SingleOperation {oAs = as @ Attributes {aReads = areads},
                       oOpr = Natural (Linear {oUs = [(Bound (MachineFrameIndex _ _ _)), _, _, _, _]})}
  = let areads' = [OtherSideEffect RSP] ++ areads
    in o {oAs = as {aReads = areads'}}

stackIndexReadsSP
  o @ SingleOperation {oAs = as @ Attributes {aReads = areads},
                       oOpr = Natural (Linear {oUs = [_, (Bound (MachineFrameIndex _ _ _)), _, _, _, _]})}
  = let areads' = [OtherSideEffect RSP] ++ areads
    in o {oAs = as {aReads = areads'}}

stackIndexReadsSP o = o
