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
     handleStackOperands) where

import MachineIR
import Unison
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
    FIXME: for IMUL64m, writesSideEffect = [rax,rdx,eflags] (expected), readsSideEffect = [mem-mem] (WRONG)
    FIXME: find and augment the actual operands (last n uses, last m reads?)
-}

handlePromotedOperands
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = [TargetInstruction i], oDs = defs, oUs = uses})} |
  let
    ws  = filter (writesSideEffect i) promotedRegs
    rs  = filter (readsSideEffect i) promotedRegs
  in {- trace ("handlePromotedOperands " ++ show o ++ " " ++ show ws ++ " " ++ show rs) -} False = undefined
handlePromotedOperands o = o

{-
    o97: [t94] <- MOV32rm [%stack.0,4,t93,0,_] (mem: 0)
frame:
    %stack.0: offset = 80, size = 12, align = 8
->
    o97: [t94] <- MOV32rm [rsp,4,t93,80,_] (mem: 0)

FIXME: consider mfiFixed attribute
-}

handleStackOperands
  o @ SingleOperation {oOpr = Natural ni @ (Linear {oUs = [(Bound MachineFrameIndex {mfiIndex = off1}),
                                                           use2,
                                                           use3,
                                                           (Bound MachineImm {miValue = off2}),
                                                           use5]})}
  = let use1' = mkRegister (mkTargetRegister RSP)
        use4' = mkBound (mkMachineImm (off1 + off2))
    in
    o {oOpr = Natural ni {oUs = [use1',use2,use3,use4',use5]}}
handleStackOperands
  o @ SingleOperation {oOpr = Natural ni @ (Linear {oUs = [(Bound MachineFrameIndex {mfiIndex = off1}),
                                                           use2,
                                                           use3,
                                                           (Bound MachineImm {miValue = off2}),
                                                           use5,
                                                           use6]})}
  = let use1' = mkRegister (mkTargetRegister RSP)
        use4' = mkBound (mkMachineImm (off1 + off2))
    in
    o {oOpr = Natural ni {oUs = [use1',use2,use3,use4',use5,use6]}}
handleStackOperands
  o @ SingleOperation {oOpr = Natural ni @ (Linear {oUs = [use1,
                                                           (Bound MachineFrameIndex {mfiIndex = off1}),
                                                           use3,
                                                           use4,
                                                           (Bound MachineImm {miValue = off2}),
                                                           use6]})}
  = let use2' = mkRegister (mkTargetRegister RSP)
        use5' = mkBound (mkMachineImm (off1 + off2))
    in
    o {oOpr = Natural ni {oUs = [use1,use2',use3,use4,use5',use6]}}
handleStackOperands o = o