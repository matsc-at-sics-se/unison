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
     transAlternativeLEA,
     liftReturnAddress,
     revertFixedFrame,
     liftStackArgSize,
     addPrologueEpilogue,
     suppressCombineCopies,
     movePrologueEpilogue,
     myLowerFrameIndices,
     addStackIndexReadsSP,
     addFunWrites,
     addVzeroupper,
     removeDeadEflags,
     moveOptWritesEflags,
     spillAfterAlign) where

import qualified Data.Map as M
import Data.List
import Data.List.Split
import qualified Data.Graph.Inductive as G
import Data.Maybe

import MachineIR
import Unison
import Unison.Analysis.FrameOffsets
import qualified Unison.Graphs.ICFG as ICFG
import qualified Unison.Graphs.BCFG as BCFG

import Unison.Target.X86.Common
import Unison.Target.X86.BranchInfo
import Unison.Target.X86.X86RegisterDecl
import Unison.Target.X86.X86RegisterClassDecl
import Unison.Target.X86.Registers
import qualified Unison.Target.X86.SpecsGen as SpecsGen
import Unison.Target.X86.SpecsGen.X86InstructionDecl

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

spillAfterAlign _ (o :
                   p @ SingleOperation {oOpr = Natural (Linear {oIs = [TargetInstruction ALIGN_SP_32]})} :
                   q @ SingleOperation {oOpr = Natural (Linear {oIs = [TargetInstruction SUBRSP_pseudo]})} :
                   rest) _
  | isCopy o
  = (rest, [p,q,o])
spillAfterAlign _ (o : rest) _ = (rest, [o])

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

-- This transform adds LEA variants of various ADD and MUL instructions, which write eflags,
-- except if there is some operation that reads those eflags.

transAlternativeLEA f @ Function {fCode = code} =
  let icfg   = ICFG.fromBCFG $ BCFG.fromFunction branchInfo' f
      writers = [id | (id, (_, o)) <- G.labNodes icfg, isEflagsWriter o]
      cedges = concat [[(p,id) | p <- G.pre icfg id] | id <- writers]
      icfg'  = G.delEdges cedges icfg
      creach = [(id, G.reachable id icfg') | id <- writers]
      lwriters = [oId $ snd (fromJust (G.lab icfg' id)) | (id, reachers) <- creach,
                                                          any (nodeIsEflagsReader icfg') reachers]
      code' = mapToOperationInBlocks (alternativeLEA lwriters) code
  in f {fCode = code'}

nodeIsEflagsReader icfg id =
  isEflagsReader $ snd (fromJust (G.lab icfg id))

isEflagsWriter o =
  any insnWritesEflags (oInstructions o)

insnWritesEflags TargetInstruction {oTargetInstr = i} =
  let (_, ws) = SpecsGen.readWriteInfo i
  in (OtherSideEffect EFLAGS) `elem` ws
insnWritesEflags _ = False

isEflagsReader o =
  any insnReadsEflags (oInstructions o)

insnReadsEflags TargetInstruction {oTargetInstr = i} =
  let (rs, _) = SpecsGen.readWriteInfo i
  in (OtherSideEffect EFLAGS) `elem` rs
insnReadsEflags _ = False

alternativeLEA precious o
  | (oId o) `elem` precious
  = o

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = [TargetInstruction ti]})}
  | ti `elem` [ADD32ri, ADD32ri8, ADD32ri8_DB, ADD32ri_DB]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction ADD32ri_LEA]}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = [TargetInstruction ti]})}
  | ti `elem` [ADD32rr, ADD32rr_DB, ADD32rr_REV]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction ADD32rr_LEA]}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = [TargetInstruction ti]})}
  | ti `elem` [ADD64ri8, ADD64ri8_DB, ADD64ri32, ADD64ri32_DB]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction ADD64ri_LEA]}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = [TargetInstruction ti]})}
  | ti `elem` [ADD64rr, ADD64rr_DB, ADD64rr_REV]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction ADD64rr_LEA]}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = [TargetInstruction ti]})}
  | ti `elem` [SHL32r1]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction SHL32r1_LEA]}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = [TargetInstruction ti], oUs = [_,Bound (MachineImm sh)]})}
  | ti `elem` [SHL32ri] && 1 <= sh && sh <= 3
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction SHL32ri_LEA]}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = [TargetInstruction ti]})}
  | ti `elem` [SHL64r1]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction SHL64r1_LEA]}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = [TargetInstruction ti], oUs = [_,Bound (MachineImm sh)]})}
  | ti `elem` [SHL64ri] && 1 <= sh && sh <= 3
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction SHL64ri_LEA]}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = [TargetInstruction ti]})}
  | M.member ti condMoveAlts
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction (condMoveAlts M.! ti)]}}

alternativeLEA _ o = o

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

-- Detect stack space needed for passing arguments in calls.

liftStackArgSize f @ Function {fCode = code} =
  let flatcode = (flatten code)
      rcs = concatMap rspCopy flatcode
      mn = maximum $ map (liftStackArgSize' rcs) flatcode
  in f {fStackArgSize = mn}

rspCopy SingleOperation {oOpr = Virtual (VirtualCopy {oVirtualCopyD = d,
                                                      oVirtualCopyS = (Register (TargetRegister RSP))})}
  = [d]
rspCopy _ = []

liftStackArgSize' rcs o
 | (length $ oUses o) == 6 && (head $ oUses o) `elem` rcs
 = let (SingleOperation {oOpr = Natural (Linear {oUs = us, oIs = [TargetInstruction ti]})}) = o
       w = instructionMemWidth ti
       [_,_,_,offp,_,_] = us
       (Bound (MachineImm off)) = offp
   in off + w
liftStackArgSize' _ _ = 0

instructionMemWidth ti
  | ti `elem` [MOV8mi, MOV8mr] = 1
  | ti `elem` [MOV16mi, MOV16mr] = 2
  | ti `elem` [MOV32mi, MOV32mr] = 4
  | ti `elem` [MOV64mi32, MOV64mr] = 8
  | ti `elem` [VMOVAPSYmr] = 32
  | True = error ("unmatched: instructionMemWidth " ++ show ti)

-- This transform inserts Prologue/Epilogue, either simple or complex,
-- if alignment by more than 16 is required.

-- We need a stack frame iff there are spills or stack-allocated data.
-- Additionally, we need to ensure that SP is modulo 16 aligned at
-- all recursive calls!  At any rate, the frame size needs to be a multiple of 8.

addPrologueEpilogue f @ Function {fCode = code, fStackFrame = objs} =
  let flatcode = flatten code
      align    = if any isDirtyYMMOp flatcode then 32 else 8
      align'   = maximum $ [align] ++ map foAlignment (objs)
      (addPrf, addEpf) = if align' > 16
                         then (addComplexPr, addComplexEp)
                         else (addSimplePr, addSimpleEp)
      ids      = newIndexes flatcode
      tid      = case ids of
                  (tid', _, _) -> tid'
      code'    = mapToEntryBlock (addPrf tid ids) code
      outBs    = returnBlockIds code'
      code''   = foldl (addEpilogueInBlock (addEpf tid)) code' outBs
  in f {fCode = code''}

addEpilogueInBlock aef code l =
    let ids   = newIndexes $ flatten code
        code' = mapToBlock (aef ids) l code
    in code'

mkSubSp oid =
  mkLinear oid [TargetInstruction SUBRSP_pseudo] [Bound mkMachineFrameSize] []

splitEpilogue code =
  split (keepDelimsL $ whenElt (\o -> isBranch o || isTailCall o)) code

mkReg = mkRegister . mkTargetRegister

addSimplePr _ (_, oid, _) (e:code) = [e, mkSubSp oid] ++ code

addSimpleEp _ (_, oid, _) code =
  let addSp  = mkLinear oid [TargetInstruction ADDRSP_pseudo]
               [Bound mkMachineFrameSize] []
      addSp' = makeSplitBarrier addSp
      [code', e] = splitEpilogue code
  in code' ++ [addSp'] ++ e

addComplexPr tid (_, oid, _) (e:code) =
  let mov64 = mkLinear oid       [TargetInstruction MOV_FROM_SP] [] [mkPreAssignedTemp tid (mkReg RBP)]
      and64 = mkLinear (oid + 1) [TargetInstruction ALIGN_SP_32] [] []
      subSp = mkSubSp  (oid + 2)
  in [e, mov64, and64, subSp] ++ code

addComplexEp tid (_, oid, _) code =
  let mov64  = mkLinear oid [TargetInstruction MOV_TO_SP] [mkTemp tid] []
      mov64' = makeSplitBarrier mov64
      [code', e] = splitEpilogue code
  in code' ++ [mov64'] ++ e

makeSplitBarrier = mapToAttrSplitBarrier (const True)

-- This transformation suppresses unsafe 32-bit copies in the first argument of (combine),
-- because instructions with a 32-bit destination implicitly zeroes the upper 32 bits.

suppressCombineCopies f @ Function {fCode = code} =
  let t2h = M.fromList $ concatMap definerInsns (flatten code)
      code' = mapToOperationInBlocks (suppressCombineCopies' t2h) code
  in f {fCode = code'}

definerInsns o =
  let insns = oInstructions o
      hazard = (TargetInstruction MOVE32) `elem` insns
  in [(mkTemp t, hazard) | t <- defTemporaries [o]]

suppressCombineCopies' t2h o @ SingleOperation {oOpr = Virtual (co @ Combine {oCombineLowU = p @ MOperand {altTemps = alts}})}
  = let alts' = [t | t <- alts, not (t2h M.! t)]
    in o {oOpr = Virtual (co {oCombineLowU = p {altTemps = alts'}})}
suppressCombineCopies' _ o = o

-- This transform prevents any STORE* from occurring before the prologue and any LOAD* from occurring after the epilogue.
-- Note that a LOAD* can have a _remat alternative, which could write dead eflags.
-- Note that STORE* can be mixed with PUSH and LOAD* can be mixed with POP*.

movePrologueEpilogue f @ Function {fCode = code} =
  let outBs    = returnBlockIds code
      code'    = mapToEntryBlock (movePrf []) code
      code''   = foldl (moveEpilogueInBlock moveEpf) code' outBs
  in f {fCode = code''}

moveEpilogueInBlock aef code l =
    mapToBlock aef l code

movePrf moves (o:code)
  | isCopy o && (oWriteObjects o) == []
  = movePrf (moves ++ [o]) code
movePrf moves (o @ SingleOperation {oOpr = Natural (Linear {oIs = [TargetInstruction SUBRSP_pseudo]})}
              : code)
  = [o] ++ moves ++ code
movePrf moves (o:code)
  = [o] ++ movePrf moves code

moveEpf code =
  let [code', (epi : code'')] = split (keepDelimsL $ whenElt (\o -> isEpi o)) code
  in code' ++ moveEpf' [] epi code''

moveEpf' pops epi (o:code)
  | isCopy o && (oWriteObjects o) == []
  = [o] ++ moveEpf' pops epi code
moveEpf' pops epi (o:code)
  | isCopy o && (oWriteObjects o) == [OtherSideEffect EFLAGS]
  = [o] ++ moveEpf' pops epi code
moveEpf' pops epi (o:code)
  | isCopy o
  = moveEpf' (pops ++ [o]) epi code
moveEpf' pops epi code
  = [epi] ++ pops ++ code

isEpi SingleOperation {oOpr = Natural (Linear {oIs = [TargetInstruction ti]})} =
  ti `elem` [ADDRSP_pseudo, MOV_TO_SP]
isEpi _ = False

-- This transform replaces stack frame object indices in the code by actual
-- RSP + immediates.  It essentially overrules lowerFrameIndices.

myLowerFrameIndices f @ Function {fCode = code,
                                  fFixedStackFrame = fobjs,
                                  fStackFrame = objs,
                                  fStackArgSize = sasize} =
  let done     = negate $ minimum $ map foOffset fobjs
      need     = negate $ minimum $ map foOffset (fobjs ++ objs)
      need'    = need + sasize
      flatcode = (flatten code)
      align    = if any isDirtyYMMOp flatcode then 32 else if any isCall flatcode then 16 else 8
      align'   = maximum $ [align] ++ map foAlignment (objs)
  in myLowerFrameIndices' need' done align' f

myLowerFrameIndices' need done align f @ Function {fCode = code, fFixedStackFrame = fobjs,
                                                   fStackFrame = objs} | align < 32 =
  let need'    = ((((need-1) `div` align) + 1) * align)
      code'    = replaceFIsByImms need' done (mkTargetRegister RSP) True fobjs code
      code''   = replaceFIsByImms need' done (mkTargetRegister RSP) False objs code'
  in f {fCode = code''}
myLowerFrameIndices' need done align f @ Function {fCode = code, fFixedStackFrame = fobjs,
                                                   fStackFrame = objs} =
  let need'    = ((((need-1) `div` align) + 1) * align)
      code'    = replaceFIsByImms done  done  (mkTargetRegister RBP) True fobjs code
      code''   = replaceFIsByImms need'    0  (mkTargetRegister RSP) False objs code'
  in f {fCode = code''}

replaceFIsByImms need done basereg fixed objs code =
  let idxToOff = M.fromList [(foIndex fo, (foOffset fo) + need) | fo <- objs]
  in mapToOperationInBlocks
     (liftFIif basereg fixed (need - done) idxToOff) code

liftFIif basereg fixed decr idxToOff (Bundle os) =
  mkBundle $ map (liftFIif basereg fixed decr idxToOff) os

liftFIif _ False decr _
  o @ SingleOperation {oOpr = Natural ni @ (Linear {oIs = [TargetInstruction i]})}
  | i `elem` [SUBRSP_pseudo, ADDRSP_pseudo]
  = let use1' = mkBound (mkMachineImm decr)
    in o {oOpr = Natural ni {oUs = [use1']}}
liftFIif basereg fixed _ idxToOff
  o @ SingleOperation {oOpr = Natural ni @ (Linear {oUs = [(Bound (MachineFrameIndex idx fixed' off1)),
                                                           use2,
                                                           use3,
                                                           (Bound (MachineImm off2)),
                                                           use5]})}
  | fixed == fixed'
  = let use1' = mkRegister basereg
        use4' = mkBound (mkMachineImm $ (idxToOff M.! idx) + off1 + off2)
    in o {oOpr = Natural ni {oUs = [use1',use2,use3,use4',use5]}}
liftFIif basereg fixed _ idxToOff
  o @ SingleOperation {oOpr = Natural ni @ (Linear {oUs = [(Bound (MachineFrameIndex idx fixed' off1)),
                                                           use2,
                                                           use3,
                                                           (Bound (MachineImm off2)),
                                                           use5,
                                                           use6]})}
  | fixed == fixed'
  = let use1' = mkRegister basereg
        use4' = mkBound (mkMachineImm $ (idxToOff M.! idx) + off1 + off2)
    in o {oOpr = Natural ni {oUs = [use1',use2,use3,use4',use5,use6]}}
liftFIif basereg fixed _ idxToOff
  o @ SingleOperation {oOpr = Natural ni @ (Linear {oUs = [use1,
                                                           (Bound (MachineFrameIndex idx fixed' off1)),
                                                           use3,
                                                           use4,
                                                           (Bound (MachineImm off2)),
                                                           use6]})}
  | fixed == fixed'
  = let use2' = mkRegister basereg
        use5' = mkBound (mkMachineImm $ (idxToOff M.! idx) + off1 + off2)
    in o {oOpr = Natural ni {oUs = [use1,use2',use3,use4,use5',use6]}}
liftFIif _ _ _ _ o = o

addStackIndexReadsSP
  o @ SingleOperation {oAs = as @ Attributes {aReads = areads},
                       oOpr = Natural (Linear {oUs = [(Bound (MachineFrameIndex _ _ _)), _, _, _, _]})}
  = let areads' = [OtherSideEffect RSP] ++ areads
    in o {oAs = as {aReads = areads'}}

addStackIndexReadsSP
  o @ SingleOperation {oAs = as @ Attributes {aReads = areads},
                       oOpr = Natural (Linear {oUs = [_, (Bound (MachineFrameIndex _ _ _)), _, _, _, _]})}
  = let areads' = [OtherSideEffect RSP] ++ areads
    in o {oAs = as {aReads = areads'}}

addStackIndexReadsSP o = o

addFunWrites o
  | isFun o
  = mapToWrites (++ [OtherSideEffect EFLAGS]) o
addFunWrites o = o

-- This transformation adds VZEROUPPER instructions right before calls and returns
-- after stretches of code that touches any YMM register.

addVzeroupper f @ Function {fCode = code} =
  let icfg   = ICFG.fromBCFG $ BCFG.fromFunction branchInfo' f
      cnodes = [id | (id, (_, o)) <- G.labNodes icfg, isCall o || isTailCall o]
      rnodes = [id | (id, (_, o)) <- G.labNodes icfg,
                (TargetInstruction RETQ) `elem` oInstructions o]
      cedges = concat [[(id, s) | s <- G.suc icfg id] | id <- cnodes]
      icfg'  = G.delEdges cedges icfg
      icfgr  = G.grev icfg'
      callFuns = filter (\o -> isCall o || isTailCall o || isFun o) (flatten code)
      cnodes' = filter (insertCandidate icfg callFuns) cnodes
      creach = [(id, G.reachable id icfgr) | id <- cnodes' ++ rnodes]
      ymmreach = [snd (fromJust (G.lab icfg id)) | (id, reachers) <- creach,
                  any (isYMMDirtying icfg) reachers]
      code'  = foldl insertVzeroupper code ymmreach
  in f {fCode = code'}

isYMMDirtying icfg id =
  isDirtyYMMOp $ snd (fromJust (G.lab icfg id))

branchInfo' bo @ SingleOperation {oOpr = Natural i}
  | isBranch bo = Just (branchInfo i)
  | otherwise = Nothing

insertVzeroupper code o =
  let (_, oid, _) = newIndexes $ flatten code
      vzu = mkLinear oid [TargetInstruction VZEROUPPER] [] []
      code' = map (insertOperationInBlock before (isIdOf o) vzu) code
   in code'

insertCandidate icfg callFuns id =
  let o = snd (fromJust (G.lab icfg id))
  in relevantFunAfter callFuns o

relevantFunAfter [] _ = True

relevantFunAfter (call : fun : _) o
  | call == o
  = let (SingleOperation {oOpr = Virtual (Fun {oFunctionUs = funuses})}) = fun
    in not (any ymmPreassigned funuses)

relevantFunAfter (_ : rest) o
  = relevantFunAfter rest o

ymmPreassigned p =
  case preAssignment p of
    Nothing -> False
    Just (Register (TargetRegister r)) -> r `elem` registers (RegisterClass VR256)

-- In order to remove many false dependencies, this transform juggles EFLAGS side-effects:
-- reads eflags -> writes eflags
-- writes eflags, LIVE -> writes eflags
-- writes eflags, DEAD -> reads eflags

removeDeadEflags f @ Function {fCode = code} =
  let icfg   = ICFG.fromBCFG $ BCFG.fromFunction branchInfo' f
      srcids = [id | (id, (_, o)) <- G.labNodes icfg, isMandatory o, oWritesEflags o]
      sinkids = [id | (id, (_, o)) <- G.labNodes icfg, oReadsEflags o]
      srcedges = concat [[(p,id) | p <- G.pre icfg id] | id <- srcids]
      sinkedges = concat [[(id,s) | s <- G.suc icfg id] | id <- sinkids]
      icfg'  = G.delEdges (srcedges ++ sinkedges) icfg
      creach = [(id, G.reachable id icfg') | id <- srcids]
      livesrcs = [oId $ snd (fromJust (G.lab icfg' id)) | (id, reachers) <- creach,
                                                          any (nodeReadsEflags icfg') reachers]
      code' = mapToOperationInBlocks (removeDeadEflags' livesrcs) code
  in f {fCode = code'}

removeDeadEflags' livesrcs o
  | (oId o) `elem` livesrcs
  = o

removeDeadEflags' _ o
  | oReadsEflags o
  = oFlipReadEflags o

removeDeadEflags' _ o
  | oWritesEflags o
  = oFlipWriteEflags o

removeDeadEflags' _ o = o

nodeReadsEflags icfg id =
  oReadsEflags $ snd (fromJust (G.lab icfg id))

oReadsEflags (SingleOperation {oAs = Attributes {aReads = rs}})
  = (OtherSideEffect EFLAGS) `elem` rs

oWritesEflags (SingleOperation {oAs = Attributes {aWrites = ws}})
  = (OtherSideEffect EFLAGS) `elem` ws

oFlipReadEflags o @ (SingleOperation {oAs = atts @ Attributes {aReads = rs, aWrites = ws}})
  = let rs' = delete (OtherSideEffect EFLAGS) rs
        ws' = [OtherSideEffect EFLAGS] ++ ws
  in o {oAs = atts {aReads = rs', aWrites = ws'}}

oFlipWriteEflags o @ (SingleOperation {oAs = atts @ Attributes {aReads = rs, aWrites = ws}})
  = let ws' = delete (OtherSideEffect EFLAGS) ws
        rs' = [OtherSideEffect EFLAGS] ++ rs
  in o {oAs = atts {aReads = rs', aWrites = ws'}}

-- If there is some static sequence:
--   Mandatory writes eflags
--   [...]
--   Optional  writes eflags
--   [...]
--   Reads eflags
-- then any such optional ops are made to precede the mandatory write

moveOptWritesEflags f @ Function {fCode = code} =
  let ls    = map bLab code
      code' = foldl (moveOptWritesEflags' moveOptW) code ls
  in f {fCode = code'}

moveOptWritesEflags' aef code l =
    mapToBlock aef l code

moveOptW [] = []

moveOptW (o:code)
  | oWritesEflags o
  = moveOptW' [] [o] code

moveOptW (o:code) =
  [o] ++ moveOptW code

moveOptW' hazard others []
  = hazard ++ others

moveOptW' hazard others (o:code)
  | oWritesEflags o || isDelimiter o
  = hazard ++ others ++ [o] ++ moveOptW code

moveOptW' hazard others (o:code)
  | oReadsEflags o
  = moveOptW' (hazard ++ [o]) others code

moveOptW' hazard others (o:code)
  = moveOptW' hazard (others ++ [o]) code
