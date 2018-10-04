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
    (disambiguateFunction,
     mkConsistentFunction,
     addRemat64bit,
     cleanFunRegisters,
     promoteImplicitOperands,
     expandPseudos,
     demoteImplicitOperands,
     addImplicitRegs,
     ambiguateRegs,
     extractReturnRegs,
     handlePromotedOperands,
     generalizeRegisterDefines,
     generalizeRegisterUses,
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
     addYMMReads,
     addSpillIndicators,
     addVzeroupper,
     removeDeadEflags) where

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


-- This transformation disambiguates XMM* (%xmm*) regs as UMM*, VMM*, or WMM*

disambiguateFunction mf @ MachineFunction {mfBlocks = blocks} =
  let tid2rc = registerClassMap mf
      blocks' = map (disambiguateBlock tid2rc) blocks
  in mf {mfBlocks = blocks'}

disambiguateBlock tid2rc b @ MachineBlock {mbInstructions = insns} =
  let insns'  = disambiguateBlockBwd tid2rc (reverse insns)
      insns'' = disambiguateBlockFwd tid2rc (reverse insns')
  in b {mbInstructions = insns''}

disambiguateBlockBwd tid2rc insns =
  let (_, insns') = mapAccumL (disambiguateBwd tid2rc) M.empty insns
  in insns'

disambiguateBwd tid2rc amb2det mi @ MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COPY,
                                                   msOperands = [p @ MachineTemp {mtId = did}, MachineReg {mrName = r}]}
  | isAmbigReg r =
  let w =
        case (tid2rc M.! did) of 
          "fr32"     -> 4
          "fr64"     -> 8
          "fr128"    -> 16
          "vr128"    -> 16
          _          -> error ("disambiguateBwd: missing register class " ++ show (tid2rc M.! did))
      r' = disAmbigReg w r
      mi' = mi {msOperands = [p, mkMachineReg r']}
      amb2det' = M.insert r r' amb2det
  in (amb2det', mi')

disambiguateBwd _ amb2det mi @ MachineSingle {msOpcode = MachineVirtualOpc MachineIR.ENTRY,
                                              msOperands = ps} =
  let ps' = map (disambiguateOperand amb2det) ps
      mi' = mi {msOperands = ps'}
  in (amb2det, mi')

disambiguateBwd _ amb2det mi @ MachineSingle {msOpcode = MachineVirtualOpc FUN,
                                              msOperands = ps} =
  let ps' = map (disambiguateFunOperand amb2det MachineRegImplicitDefine) ps
      mi' = mi {msOperands = ps'}
  in (amb2det, mi')

disambiguateBwd _ amb2det mi = (amb2det, mi)

disambiguateBlockFwd tid2rc insns =
  let (_, insns') = mapAccumL (disambiguateFwd tid2rc) M.empty insns
  in insns'

disambiguateFwd tid2rc amb2det mi @ MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COPY,
                                                   msOperands = [MachineReg {mrName = r}, p @ MachineTemp {mtId = did}]}
  | isAmbigReg r =
  let w =
        case (tid2rc M.! did) of 
          "fr32"     -> 4
          "fr64"     -> 8
          "fr128"    -> 16
          "vr128"    -> 16
          _          -> error ("disambiguateFwd: missing register class " ++ show (tid2rc M.! did))
      r' = disAmbigReg w r
      mi' = mi {msOperands = [mkMachineReg r', p]}
      amb2det' = M.insert r r' amb2det
  in (amb2det', mi')

disambiguateFwd _ amb2det mi @ MachineSingle {msOpcode = MachineVirtualOpc FUN,
                                              msOperands = ps} =
  let ps' = map (disambiguateFunOperand amb2det MachineRegImplicit) ps
      mi' = mi {msOperands = ps'}
  in (amb2det, mi')

disambiguateFwd _ amb2det mi @ MachineSingle {msOpcode = MachineVirtualOpc RETURN,
                                              msOperands = ps} =
  let ps' = map (disambiguateOperand amb2det) ps
      mi' = mi {msOperands = ps'}
  in (amb2det, mi')

disambiguateFwd _ amb2det mi @ MachineSingle {msOpcode = MachineTargetOpc RETQ,
                                              msOperands = ps} =
  let ps' = map (disambiguateOperand amb2det) ps
      mi' = mi {msOperands = ps'}
  in (amb2det, mi')

disambiguateFwd _ amb2det mi = (amb2det, mi)

disambiguateOperand amb2det p @ MachineReg {mrName = r}
  | M.member r amb2det =
  p {mrName = amb2det M.! r}

disambiguateOperand _ p = p

disambiguateFunOperand amb2det f p @ MachineReg {mrName = r, mrFlags = fs}
  | M.member r amb2det && f `elem` fs =
  p {mrName = amb2det M.! r}

disambiguateFunOperand _ _ p = p

-- This transformation barfs on inconsistent float arguments, eventually modifying the opcode

mkConsistentFunction mf @ MachineFunction {mfBlocks = blocks} =
  let tid2rc   = registerClassMap mf
      id    = newMachineTempId mf
      (_, blocks') = mapAccumL (mkConsistentBlock tid2rc) id blocks
  in mf {mfBlocks = blocks'}


mkConsistentBlock tid2rc id b @ MachineBlock {mbInstructions = insns} =
  let (id', insns')  = mapAccumL (mkConsistentMI tid2rc) id insns
      insns'' = concat insns'
  in (id', b {mbInstructions = insns''})

mkConsistentMI tid2rc id MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COPY,
                                           msOperands = [MachineTemp {mtId = did}, MachineTemp {mtId = uid}]}
  | M.member did tid2rc && M.member uid tid2rc =
  let drc = tid2rc M.! did
      urc = tid2rc M.! uid
      dsz = safeGet "regClassStringToLogSize" regClassStringToLogSize drc
      usz = safeGet "regClassStringToLogSize" regClassStringToLogSize urc
  in castCopy (dsz - usz) id did uid

mkConsistentMI tid2rc id mi @ MachineSingle {msOpcode = MachineTargetOpc i, msOperands = ps}
  | i /= RETQ && instructionType i /= TailCallInstructionType && any isMachineTemp ps =
  let (uif,dif) = SpecsGen.operandInfo i
      nbImp    = length $ filter promotedImplicit    ps
      nbImpDef = length $ filter promotedImplicitDef ps
      nbUif    = length uif
      nbDif    = length dif
      uif'     = take (nbUif - nbImp) uif
      dif'     = take (nbDif - nbImpDef) dif
      (da,ua) = splitAt (length dif') [p | p <- ps, explicitOperand p]
  in if (length da) /= (length dif') || (length ua) /= (length uif')
     then error ("wrong number of operands: " ++ show mi) (id, [mi])
     else mkConsistentMI' tid2rc id dif' da uif' ua mi

mkConsistentMI _ id mi = (id, [mi])

promotedImplicitDef MachineReg {mrName = r, mrFlags = [MachineRegImplicitDefine]} = not (r `elem` [EFLAGS, RSP])
promotedImplicitDef _ = False

promotedImplicit MachineReg {mrName = r, mrFlags = [MachineRegImplicit]} = not (r `elem` [EFLAGS, RSP])
promotedImplicit _ = False

explicitOperand MachineReg {mrFlags = [f]} = not (f `elem` [MachineRegImplicit, MachineRegImplicitDefine])
explicitOperand _ = True

mkConsistentMI' tid2rc id dif da uif ua mi @ MachineSingle {msOperands = ps} =
  let dplan = map (planCast tid2rc False) (zip dif da)
      uplan = map (planCast tid2rc True) (zip uif ua)
      ((id',trans'), post) = mapAccumL (applyCast False) (id, M.empty) dplan
      ((id'',trans''), pre) = mapAccumL (applyCast True) (id', trans') uplan
      ps' = map (applyTrans trans'') ps
  in (id'', (concat pre) ++ [mi {msOperands = ps'}] ++ (concat post))

planCast tid2rc isUse (TemporaryInfo {oiRegClass = (RegisterClass frc)}, MachineTemp {mtId = tid}) =
  let arc = (tid2rc M.! tid)
      fsz = safeGet "regClassToLogSize" regClassToLogSize frc
      asz = safeGet "regClassStringToLogSize" regClassStringToLogSize arc
      dif = if isUse then fsz-asz else asz-fsz
  in (tid,dif)

planCast _ _ _ = (0,0)

applyCast _ (id,trans) (_, 0) = ((id,trans), [])

-- given a use operand %tid
-- generate cast code from %tid to %id
-- change the operand to %id
applyCast isUse (id, trans) (tid, dif) | isUse =
  let (id', code) = castCopy dif (id+1) id tid
      trans' = M.insert tid id trans
  in ((id', trans'), code)

-- given a def operand %tid
-- generate cast code from %id to %tid
-- change the operand to %id
applyCast isUse (id, trans) (tid, dif) | not isUse =
  let (id', code) = castCopy dif (id+1) tid id
      trans' = M.insert tid id trans
  in ((id', trans'), code)

applyTrans trans MachineTemp {mtId = tid}
  | M.member tid trans =
  mkSimpleMachineTemp (trans M.! tid)

applyTrans _ p = p

safeGet label m key =
  case M.lookup key m of
  Just v -> v
  Nothing -> error ("missing key " ++ show key ++ " in map " ++ label)

castCopy (-3) id did uid =
  let mi1 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.LOW,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp id, mkSimpleMachineTemp uid]}
      mi2 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.LOW,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp (id+1), mkSimpleMachineTemp id]}
      mi3 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.LOW,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp did, mkSimpleMachineTemp (id+1)]}
  in (id+2, [mi1, mi2, mi3])

castCopy (-2) id did uid =
  let mi1 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.LOW,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp id, mkSimpleMachineTemp uid]}
      mi2 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.LOW,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp did, mkSimpleMachineTemp id]}
  in (id+1, [mi1, mi2])

castCopy (-1) id did uid =
  let mi1 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.LOW,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp did, mkSimpleMachineTemp uid]}
  in (id, [mi1])

castCopy (0) id did uid =
  let mi1 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COPY,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp did, mkSimpleMachineTemp uid]}
  in (id, [mi1])

castCopy 1 id did uid =
  let mi1 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.IMPLICIT_DEF,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp id]}
      mi2 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COMBINE,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp did, mkSimpleMachineTemp uid, mkSimpleMachineTemp id]}
  in (id+1, [mi1, mi2])

castCopy 2 id did uid =
  let mi1 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.IMPLICIT_DEF,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp id]}
      mi2 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COMBINE,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp (id+1), mkSimpleMachineTemp uid, mkSimpleMachineTemp id]}
      mi3 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.IMPLICIT_DEF,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp (id+2)]}
      mi4 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COMBINE,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp did, mkSimpleMachineTemp (id+1), mkSimpleMachineTemp (id+2)]}
  in (id+3, [mi1, mi2, mi3, mi4])

castCopy 3 id did uid =
  let mi1 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.IMPLICIT_DEF,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp id]}
      mi2 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COMBINE,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp (id+1), mkSimpleMachineTemp uid, mkSimpleMachineTemp id]}
      mi3 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.IMPLICIT_DEF,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp (id+2)]}
      mi4 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COMBINE,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp (id+3), mkSimpleMachineTemp (id+1), mkSimpleMachineTemp (id+2)]}
      mi5 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.IMPLICIT_DEF,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp (id+4)]}
      mi6 = MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COMBINE,
                           msProperties = [],
                           msOperands = [mkSimpleMachineTemp did, mkSimpleMachineTemp (id+3), mkSimpleMachineTemp (id+4)]}
  in (id+5, [mi1, mi2, mi3, mi4, mi5, mi6])


-- This transformation finds opportunities to use a rematerializable zero-extended-to-64-bits load immediate

addRemat64bit mf @ MachineFunction {mfBlocks = blocks} =
  let tempOccs = group $ sort $ funTempOccs mf
      tempOccs' = M.fromList [(head g, length g) | g <- tempOccs]
      blocks' = map (addRemat64bitBlock tempOccs') blocks
  in mf {mfBlocks = blocks'}

addRemat64bitBlock tempOccs b @ MachineBlock {mbInstructions = insns} =
  let insns'  = addRemat64bitScan tempOccs insns
  in b {mbInstructions = insns'}

addRemat64bitScan tempOccs ( mi1 @ MachineSingle {msOpcode = MachineTargetOpc MOV32ri64,
                                                  msOperands = [t1,con]}
                           : MachineSingle {msOpcode = MachineVirtualOpc MachineIR.IMPLICIT_DEF}
                           : MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COMBINE,
                                            msOperands = [t2,_,_]}
                           : rest
                           )
  | tempOccs M.! (fromInteger (mtId t1)) == 2 =
  let mi1' = mi1 {msOpcode = MachineTargetOpc MOV64ri64,
                  msOperands = [t2,con]}
  in (mi1' : addRemat64bitScan tempOccs rest)

addRemat64bitScan tempOccs ( mi1 @ MachineSingle {msOpcode = MachineTargetOpc MOV32r0,
                                                  msOperands = (t1 : _)}
                           : MachineSingle {msOpcode = MachineVirtualOpc MachineIR.IMPLICIT_DEF}
                           : MachineSingle {msOpcode = MachineVirtualOpc MachineIR.COMBINE,
                                            msOperands = [t2,_,_]}
                           : rest
                           ) 
  | tempOccs M.! (fromInteger (mtId t1)) == 2 =
  let mi1' = mi1 {msOpcode = MachineTargetOpc MOV64r0,
                  msOperands = [t2]}
  in (mi1' : addRemat64bitScan tempOccs rest)

addRemat64bitScan tempOccs (mi : rest) =
  mi : addRemat64bitScan tempOccs rest

addRemat64bitScan _ [] = []


funTempOccs mf = concat [miTempOccs mi | mi <- flattenMachineFunction mf]

miTempOccs mi = [id | MachineTemp {mtId = id} <- msOperands mi]



-- This transformation hides the stack pointer as a use and definition of
-- function calls.

cleanFunRegisters
 mi @ MachineSingle {msOpcode = MachineVirtualOpc FUN, msOperands = mos} =
    let mos' = [mr | mr @ MachineReg {mrName = r} <- mos, r /= RSP]
    in mi {msOperands = mos'}

cleanFunRegisters mi = mi

-- This transformation adds implicit uses and definitions that have been
-- promoted with specsgen. The order of the operands is:
-- 1. explicit defs
-- 2. implicit defs
-- 3. explicit uses
-- 4. implicit uses
-- 5. rest (for example, memory annotations to be lifted)
-- The transformation relies on the register lifting functionality in 'uni
-- import' to lift the added implicit uses and definitions to temporaries.

promoteImplicitOperands
  mi @ MachineSingle {msOpcode = MachineTargetOpc i, msOperands = mos} =
    let mos' = promoteImplicitRegs i promotedRegs mos
    in mi {msOperands = mos'}

promoteImplicitOperands mi = mi

promoteImplicitRegs i regs mos =
  let oif = SpecsGen.operandInfo i
      ws  = filter (writesSideEffect i) regs
      firstImpDef = length (snd oif) - length ws
      (eds, mos1) = splitAt firstImpDef mos
      rs  = filter (readsSideEffect i) regs
      firstImpUse = length (fst oif) - length rs
      (eus, mos2) = splitAt firstImpUse mos1
  in eds ++ map mkMachineReg ws ++ eus ++ map mkMachineReg rs ++ mos2

-- This transformation removes operands that are explicit according to OperandInfo
-- but in fact are implicit according to ReadWriteInfo.  It also sets MachineInstructionPropertyDefs
-- to the number of truly explicit defs, because Prepareforemission would compute the wrong value
-- otherwise.

demoteImplicitOperands mi @ MachineSingle {msOpcode = MachineTargetOpc i, msOperands = mos, msProperties = props}
  = let (uif,dif) = SpecsGen.operandInfo i
        ws  = filter (writesSideEffect i) promotedRegs
        rs  = filter (readsSideEffect i) promotedRegs
        (eds, mos') = splitAt (length dif - length ws) mos
        (_, mos'') = splitAt (length ws) mos'
        (eus, _) = splitAt (length uif - length rs) mos''
        props' = props ++ [mkMachineInstructionPropertyDefs (toInteger $ length eds)]
  in mi {msOperands = eds++eus, msProperties = props'}

-- This transformation replaces MachineIR.Transformations.AddImplicitRegs, which would otherwise
-- for every write side-effect add both MachineRegImplicit and MachineRegImplicitDef.

addImplicitRegs mi @ MachineSingle {msOpcode = MachineTargetOpc i, msOperands = mos} =
  let (uif,dif) = SpecsGen.readWriteInfo i
      imp   = [MachineReg d [mkMachineRegImplicitDefine] |
               (OtherSideEffect d) <- dif] ++
              [MachineReg u [mkMachineRegImplicit] |
               (OtherSideEffect u) <- uif]
      mos'  = mos ++ imp
  in mi {msOperands = mos'}

-- This transformation turns floating-point register names back to the size-ambiguous %xmm* notation

ambiguateRegs mi @ MachineSingle {msOperands = ps} =
  let ps' = map ambiguateOperand ps
  in mi {msOperands = ps'}

ambiguateOperand p @ MachineReg {mrName = r}
  | isFloatReg r =
  p {mrName = ambiguateReg r}

ambiguateOperand p = p

-- This transformation expands pseudo instructions to real instructions.

expandPseudos to = mapToMachineBlock (expandBlockPseudos (expandPseudo to))

expandPseudo _ (MachineSingle {msOpcode = MachineTargetOpc i})
  | i `elem` [SPILL32, SPILL, NOFPUSH, NOFPOP]
  = []

-- LLVM does this to optimize for size
-- expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc FPUSH,
--                                    msOperands = [_,off]}
--   | off == MachineImm 8
--   = let cx = mkMachineReg RCX
--     in [[mi {msOpcode = mkMachineTargetOpc PUSH64r,
--              msOperands = [cx]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc FPUSH,
                                   msOperands = [_,off]}
  = let sp = mkMachineReg RSP
    in [[mi {msOpcode = mkMachineTargetOpc SUB64ri8,
             msOperands = [sp, sp, off]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc FPUSH32,
                                   msOperands = [dst,off]}
  = let sp = mkMachineReg RSP
        im32 = mkMachineImm (-32)
    in [[mi {msOpcode = mkMachineTargetOpc MOV64rr,
             msOperands = [dst, sp]}],
        [mi {msOpcode = mkMachineTargetOpc AND64ri8,
             msOperands = [sp, sp, im32]}],
        [mi {msOpcode = mkMachineTargetOpc SUB64ri8,
             msOperands = [sp, sp, off]}]]

-- LLVM does this to optimize for size
-- expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc FPOP,
--                                    msOperands = [_,off]}
--   | off == MachineImm 8
--   = let cx = mkMachineReg RCX
--     in [[mi {msOpcode = mkMachineTargetOpc POP64r,
--              msOperands = [cx]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc FPOP,
                                   msOperands = [_,off]}
  = let sp = mkMachineReg RSP
    in [[mi {msOpcode = mkMachineTargetOpc ADD64ri8,
             msOperands = [sp, sp, off]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc FPOP32,
                                   msOperands = [src,_]}
  = let sp = mkMachineReg RSP
    in [[mi {msOpcode = mkMachineTargetOpc MOV64rr,
             msOperands = [sp, src]}]]

expandPseudo _ (MachineSingle {msOpcode = MachineTargetOpc PUSH_fi, msOperands = [_, s]})
  = [[mkMachineSingle (MachineTargetOpc PUSH64r) [] [s]]]

expandPseudo _ (MachineSingle {msOpcode = MachineTargetOpc POP_fi, msOperands = [d, _]})
  = [[mkMachineSingle (MachineTargetOpc POP64r) [] [d]]]

-- expand pseudos that stem from llvm; see X86ExpandPseudo.cpp

-- could do it with XOR, which would clobber EFLAGS, when it is safe, FIXME
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc MOV32r0,
                                   msOperands = [dst]}
  = let imm = mkMachineImm 0
  in [[mi {msOpcode = mkMachineTargetOpc MOV32ri,
           msOperands = [dst, imm]}]]
  -- = [[mi {msOpcode = mkMachineTargetOpc XOR32rr,
  --         msOperands = [dst, dst, dst]}]]

-- could do it with XOR + INC, which would clobber EFLAGS, when it is safe, FIXME
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc MOV32r1,
                                   msOperands = [dst]}
  = let imm = mkMachineImm 1
  in [[mi {msOpcode = mkMachineTargetOpc MOV32ri,
           msOperands = [dst, imm]}]]

-- could do it with XOR + DEC, which would clobber EFLAGS, when it is safe, FIXME
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc MOV32r_1,
                                   msOperands = [dst]}
  = let imm = mkMachineImm (-1)
  in [[mi {msOpcode = mkMachineTargetOpc MOV64ri32,
           msOperands = [machineReg32ToReg64 dst, imm]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc MOV32ri64}
  = [[mi {msOpcode = mkMachineTargetOpc MOV32ri}]]

-- candidate for LEA?
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc ADD16ri8_DB}
  = [[mi {msOpcode = mkMachineTargetOpc OR16ri8}]]

-- candidate for LEA?
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc ADD16ri_DB}
  = [[mi {msOpcode = mkMachineTargetOpc OR16ri}]]

-- candidate for LEA?
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc ADD16rr_DB}
  = [[mi {msOpcode = mkMachineTargetOpc OR16rr}]]

-- candidate for LEA?
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc ADD32ri8_DB}
  = [[mi {msOpcode = mkMachineTargetOpc OR32ri8}]]

-- candidate for LEA?
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc ADD32ri_DB}
  = [[mi {msOpcode = mkMachineTargetOpc OR32ri}]]

-- candidate for LEA?
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc ADD32rr_DB}
  = [[mi {msOpcode = mkMachineTargetOpc OR32rr}]]

-- candidate for LEA?
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc ADD64ri8_DB}
  = [[mi {msOpcode = mkMachineTargetOpc OR64ri8}]]

-- candidate for LEA?
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc ADD64ri32_DB}
  = [[mi {msOpcode = mkMachineTargetOpc OR64ri32}]]

-- candidate for LEA?
expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc ADD64rr_DB}
  = [[mi {msOpcode = mkMachineTargetOpc OR64rr}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc AVX2_SETALLONES,
                                   msOperands = [dst]}
  = [[mi {msOpcode = mkMachineTargetOpc VPCMPEQDrr,
          msOperands = [dst, dst, dst]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc AVX_SET0,
                                   msOperands = [dst]}
  = [[mi {msOpcode = mkMachineTargetOpc VXORPSYrr,
          msOperands = [dst, dst, dst]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc FsFLD0SD,
                                   msOperands = [dst]}
  = [[mi {msOpcode = mkMachineTargetOpc FsXORPDrr,
          msOperands = [dst, dst, dst]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc FsFLD0SS,
                                   msOperands = [dst]}
  = [[mi {msOpcode = mkMachineTargetOpc FsXORPSrr,
          msOperands = [dst, dst, dst]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc SETB_C8r,
                                   msOperands = [dst]}
  = [[mi {msOpcode = mkMachineTargetOpc SBB8rr,
          msOperands = [dst, dst, dst]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc SETB_C16r,
                                   msOperands = [dst]}
  = [[mi {msOpcode = mkMachineTargetOpc SBB16rr,
          msOperands = [dst, dst, dst]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc SETB_C32r,
                                   msOperands = [dst]}
  = [[mi {msOpcode = mkMachineTargetOpc SBB32rr,
          msOperands = [dst, dst, dst]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc SETB_C64r,
                                   msOperands = [dst]}
  = [[mi {msOpcode = mkMachineTargetOpc SBB64rr,
          msOperands = [dst, dst, dst]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc TEST8ri_NOREX}
  = [[mi {msOpcode = mkMachineTargetOpc TEST8ri}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc V_SET0,
                                   msOperands = [dst]}
  = [[mi {msOpcode = mkMachineTargetOpc VXORPSrr,
          msOperands = [dst, dst, dst]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc V_SETALLONES,
                                   msOperands = [dst]}
  = [[mi {msOpcode = mkMachineTargetOpc PCMPEQDrr,
          msOperands = [dst, dst, dst]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc mto,
                                   msOperands = [lab,MachineImm off]}
  | mto `elem` [TCRETURNdi, TCRETURNri, TCRETURNdi64, TCRETURNri64]
  = maybeAdjustSP off ++
    [[mi {msOpcode = mkMachineTargetOpc (expandedTailJump mto),
          msOperands = [lab]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc mto,
                                   msOperands = [a,b,c,d,e,MachineImm off]}
  | mto `elem` [TCRETURNmi, TCRETURNmi64]
  = maybeAdjustSP off ++
    [[mi {msOpcode = mkMachineTargetOpc (expandedTailJump mto),
          msOperands = [a,b,c,d,e]}]]

-- FIXME: this gives way too many false warnings
-- expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc i}
--   | SpecsGen.itinerary i == NoItinerary
--   = trace ("WARNING: missing expansion of instruction " ++ show i) [[mi]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc i} =
  case (SpecsGen.parent i) of
  Nothing -> [[mi]]
  Just i' -> [[mi {msOpcode = MachineTargetOpc i'}]]

expandedTailJump TCRETURNdi = TAILJMPd
expandedTailJump TCRETURNri = TAILJMPr
expandedTailJump TCRETURNmi = TAILJMPm
expandedTailJump TCRETURNdi64 = TAILJMPd64
expandedTailJump TCRETURNri64 = TAILJMPr64
expandedTailJump TCRETURNmi64 = TAILJMPm64

maybeAdjustSP 0 = []
maybeAdjustSP off
  | off > 0
  = let sp = mkMachineReg RSP
    in [[mkMachineSingle (mkMachineTargetOpc ADD64ri32) [] [sp, sp, mkMachineImm off]]]
maybeAdjustSP off
  | off < 0
  = let sp = mkMachineReg RSP
    in [[mkMachineSingle (mkMachineTargetOpc SUB64ri32) [] [sp, sp, mkMachineImm (-off)]]]

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

--
-- for register-defining instructions, add memory-defining variants
-- N.B. a use operand that occurs twice could be an implicit usedef
-- N.B. instructions defining a 32-bit register implicitly zeroes the 32 upper bits,
-- which is not the case for its memory-defining counterpart!
-- So we do not add such variants if the temp being defined can be used by a (combine).
-- 

generalizeRegisterDefines f @ Function {fCode = code} =
  let code' = mapToOperationInBlocks (generalizeRegisterDefines' (flatten code)) code
  in f {fCode = code'}

generalizeRegisterDefines' flatcode
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oDs = ods, oUs = ous, oIs = ois})}
  | (nub ous) == ous
  = o {oOpr = Natural ni {oIs = concatMap (genRegDef ods flatcode) ois}}

generalizeRegisterDefines' _ o = o

genRegDef ods flatcode (TargetInstruction i)
  | hasMemRegInstr i && noCombineHazard (memRegInstr i) ods flatcode
  = [TargetInstruction i, TargetInstruction (memRegInstr i)]
genRegDef _ _ ti = [ti]

noCombineHazard i _ _ | instrInfiniteUsage i /= 4 = True

noCombineHazard _ ods flatcode =
  let allusers = concatMap (flip users flatcode) ods
  in all (not . isCombine) allusers 

--
-- for register-using instructions, add memory-using variants
-- N.B. a use operand that occurs twice does not work here
--

generalizeRegisterUses f @ Function {fCode = code} =
  let code' = mapToOperationInBlocks generalizeRegisterUses' code
  in f {fCode = code'}

generalizeRegisterUses'
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oUs = ous, oIs = ois})}
  | (nub ous) == ous
  = o {oOpr = Natural ni {oIs = concatMap genRegUse ois}}

generalizeRegisterUses' o = o

genRegUse (TargetInstruction i)
  | hasRegMemInstr i
  = [TargetInstruction i, TargetInstruction (regMemInstr i)]
genRegUse ti = [ti]

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
    oOpr = Natural ni @ (Linear {oIs = (TargetInstruction ti : tis)})}
  | ti `elem` [ADD32ri, ADD32ri8, ADD32ri8_DB, ADD32ri_DB]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction ADD32ri_LEA] ++ tis}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = (TargetInstruction ti : tis)})}
  | ti `elem` [ADD32rr, ADD32rr_DB, ADD32rr_REV]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction ADD32rr_LEA] ++ tis}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = (TargetInstruction ti : tis)})}
  | ti `elem` [ADD64ri8, ADD64ri8_DB, ADD64ri32, ADD64ri32_DB]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction ADD64ri_LEA] ++ tis}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = (TargetInstruction ti : tis)})}
  | ti `elem` [ADD64rr, ADD64rr_DB, ADD64rr_REV]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction ADD64rr_LEA] ++ tis}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = (TargetInstruction ti : tis)})}
  | ti `elem` [SHL32r1]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction SHL32r1_LEA] ++ tis}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = (TargetInstruction ti : tis), oUs = [_,Bound (MachineImm sh)]})}
  | ti `elem` [SHL32ri] && 1 <= sh && sh <= 3
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction SHL32ri_LEA] ++ tis}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = (TargetInstruction ti : tis)})}
  | ti `elem` [SHL64r1]
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction SHL64r1_LEA] ++ tis}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = (TargetInstruction ti : tis), oUs = [_,Bound (MachineImm sh)]})}
  | ti `elem` [SHL64ri] && 1 <= sh && sh <= 3
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction SHL64ri_LEA] ++ tis}}

alternativeLEA _
  o @ SingleOperation {
    oOpr = Natural ni @ (Linear {oIs = (TargetInstruction ti : tis)})}
  | M.member ti condMoveAlts
  = o {oOpr = Natural ni {oIs = [TargetInstruction ti, TargetInstruction (condMoveAlts M.! ti)] ++ tis}}

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
  | ti `elem` [VMOVAPSmr, VMOVUPSmr] = 16
  | ti `elem` [VMOVAPSYmr, VMOVUPSYmr] = 32
  | True = error ("unmatched: instructionMemWidth " ++ show ti)

-- This transform inserts Prologue/Epilogue, either simple or complex,
-- if alignment by more than 16 is required.

-- We need a stack frame iff there are spills or stack-allocated data.
-- Additionally, we need to ensure that SP is modulo 16 aligned at
-- all recursive calls!  At any rate, the frame size needs to be a multiple of 8.

addPrologueEpilogue f @ Function {fCode = code} =
  let flatcode = flatten code
      ids      = newIndexes flatcode
      tid      = case ids of
                  (tid', _, _) -> tid'
      code'    = mapToEntryBlock (addPrologue tid ids) code
      outBs    = returnBlockIds code'
      code''   = foldl (addEpilogueInBlock (addEpilogue tid)) code' outBs
  in f {fCode = code''}

addEpilogueInBlock aef code l =
    let ids   = newIndexes $ flatten code
        code' = mapToBlock (aef ids) l code
    in code'

mkPush oid tid =
  mkLinear oid [TargetInstruction FPUSH32, TargetInstruction FPUSH, TargetInstruction NOFPUSH]
               [Bound mkMachineFrameSize]
               [mkTemp tid]

mkPop oid tid =
  mkLinear oid [TargetInstruction FPOP32, TargetInstruction FPOP, TargetInstruction NOFPOP]
               [mkTemp tid, Bound mkMachineFrameSize]
               []

addPrologue tid (_, oid, _) (e:code) =
  let push = mkPush oid tid
  in [e, push] ++ code

addEpilogue tid (_, oid, _) code =
  let pop  = mkPop oid tid
      pop' = makeSplitBarrier pop
      [code', tail] = splitEpilogue code
  in code' ++ [pop'] ++ tail

splitEpilogue code =
  split (keepDelimsL $ whenElt (\o -> isBranch o || isTailCall o)) code

makeSplitBarrier = mapToAttrSplitBarrier (const True)

-- This transformation suppresses unsafe 32-bit copies in the first argument of (combine),
-- because instructions with a 32-bit destination implicitly zeroes the upper 32 bits.

suppressCombineCopies f @ Function {fCode = code} =
  let t2h = M.fromList $ concatMap definerInsns (flatten code)
      code' = mapToOperationInBlocks (suppressCombineCopies' t2h) code
  in f {fCode = code'}

definerInsns o =
  let insns = oInstructions o
      hazard = (TargetInstruction IMOVE32) `elem` insns
  in [(mkTemp t, hazard) | t <- defTemporaries [o]]

suppressCombineCopies' t2h o @ SingleOperation {oOpr = Virtual (co @ Combine {oCombineLowU = p @ MOperand {altTemps = alts}})}
  = let alts' = [t | t <- alts, not (t2h M.! t)]
    in o {oOpr = Virtual (co {oCombineLowU = p {altTemps = alts'}})}
suppressCombineCopies' _ o = o

-- This transform prevents any STORE* from occurring before the prologue and any LOAD* from occurring after the epilogue.
-- Also, it puts POP_cst sequences in reverse order wrt. the PUSH_cst sequence.
-- Note that a LOAD* can have a _remat alternative, which could write dead eflags.
-- Note that STORE* can be mixed with PUSH and LOAD* can be mixed with POP*.

movePrologueEpilogue f @ Function {fCode = code} =
  let outBs    = returnBlockIds code
      code'    = mapToEntryBlock (movePrf []) code
      code''   = foldl (moveEpilogueInBlock moveEpf) code' outBs
  in f {fCode = code''}

moveEpilogueInBlock aef code l =
    mapToBlock aef l code

movePrf moves (o : code)
  | isCopy o && (oWriteObjects o) == []
  = movePrf (moves ++ [o]) code
movePrf moves (o : code)
  | isPro o
  = [o] ++ moves ++ code
movePrf moves (o:code)
  = [o] ++ movePrf moves code

moveEpf code =
  let [code', (epi : code'')] = split (keepDelimsL $ whenElt (\o -> isEpi o)) code
  in code' ++ moveEpf' [] epi code''

moveEpf' pops epi (o:code)
  | isCopy o && (oWriteObjects o) == []
  = [o] ++ moveEpf' pops epi code
-- moveEpf' pops epi (o:code) -- was meaningful for reified MOV32r0 affecting eflags, FIXME
--   | isCopy o && (oWriteObjects o) == [OtherSideEffect EFLAGS]
--   = [o] ++ moveEpf' pops epi code
moveEpf' pops epi (o:code)
  | isCopy o
  = moveEpf' (o:pops) epi code
moveEpf' pops epi code
  = [epi] ++ pops ++ code

isPro SingleOperation {oOpr = Natural (Linear {oIs = is})} =
  (TargetInstruction FPUSH) `elem` is
isPro _ = False

isEpi SingleOperation {oOpr = Natural (Linear {oIs = is})} =
  (TargetInstruction FPOP) `elem` is
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
      code'    = replaceFIsByImms done  done  (mkTargetRegister RBX) True fobjs code
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
  | i `elem` [FPUSH32, FPUSH, NOFPUSH]
  = let use1' = mkBound (mkMachineImm decr)
    in o {oOpr = Natural ni {oUs = [use1']}}

liftFIif _ False decr _
  o @ SingleOperation {oOpr = Natural ni @ (Linear {oIs = [TargetInstruction i], oUs = [use1,_]})}
  | i `elem` [FPOP32, FPOP, NOFPOP]
  = let use2' = mkBound (mkMachineImm decr)
    in o {oOpr = Natural ni {oUs = [use1,use2']}}

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

liftFIif basereg fixed _ idxToOff
  o @ SingleOperation {oOpr = Natural ni @ (Call {oCallUs = [(Bound (MachineFrameIndex idx fixed' off1)),
                                                             use2,
                                                             use3,
                                                             (Bound (MachineImm off2)),
                                                             use5]})}
  | fixed == fixed'
  = let use1' = mkRegister basereg
        use4' = mkBound (mkMachineImm $ (idxToOff M.! idx) + off1 + off2)
    in o {oOpr = Natural ni {oCallUs = [use1',use2,use3,use4',use5]}}

liftFIif basereg fixed _ idxToOff
  o @ SingleOperation {oOpr = Natural ni @ (Branch {oBranchUs = [(Bound (MachineFrameIndex idx fixed' off1)),
                                                                  use2,
                                                                  use3,
                                                                  (Bound (MachineImm off2)),
                                                                  use5]})}
  | fixed == fixed'
  = let use1' = mkRegister basereg
        use4' = mkBound (mkMachineImm $ (idxToOff M.! idx) + off1 + off2)
    in o {oOpr = Natural ni {oBranchUs = [use1',use2,use3,use4',use5]}}

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

-- This transformation adds "writes: eflags" to (fun) operations
-- because eflags behaves as a caller-saved register

addFunWrites o
  | isFun o
  = mapToWrites (++ [OtherSideEffect EFLAGS]) o
addFunWrites o = o

-- This transformation adds "reads: ymm0" to operations that use YMM registers
-- to make them precede VZEROUPPER

addYMMReads o
  | isUseYMMOp o
  = mapToReads (++ [OtherSideEffect YMM0]) o
addYMMReads o = o

-- This transformation adds SPILL32 and SPILL optional operations instructions
-- in entry and exit blocks

addSpillIndicators f @ Function {fCode = code} =
  let fcode = flatten code
      instl = concatMap oInstructions fcode
      act   = nub $ sort [TargetInstruction i | (TargetInstruction i) <- instl, i /= FSTORE256, (isStoreInstr i || isMemRegInstr i)]
      act32 = nub [TargetInstruction i | (TargetInstruction i) <- instl, i == FSTORE256]
      (_, oid, _) = newIndexes $ fcode 
      (_, code') = mapAccumL (addSpillIndicatorsToBlock act32 act) oid code
  in f {fCode = code'}

addSpillIndicatorsToBlock [] act oid b @ Block {bCode = (e : code)}
  | (isEntryBlock b || isExitBlock b)
  = let spill    = mkSpillInd act (oid + 0) SPILL
    in ((oid + 1), b {bCode = [e, spill] ++ code})

addSpillIndicatorsToBlock act32 act oid b @ Block {bCode = (e : code)}
  | (isEntryBlock b || isExitBlock b)
  = let spill32  = mkSpillInd act32 (oid + 0) SPILL32
        spill    = mkSpillInd act (oid + 1) SPILL
    in ((oid + 2), b {bCode = [e, spill32, spill] ++ code})

addSpillIndicatorsToBlock _ _ oid b = (oid, b)

mkSpillInd act oid ti =
  addActivators act $ makeOptional $ mkLinear oid [TargetInstruction ti] [] []

addActivators = mapToActivators . (++)

-- This transformation adds VZEROUPPER instructions right before calls and returns
-- after stretches of code that touches any YMM register.

addVzeroupper f @ Function {fCode = code} =
  let icfg   = ICFG.fromBCFG $ BCFG.fromFunction branchInfo' f
      cnodes = [id | (id, (_, o)) <- G.labNodes icfg, isCall o || isTailCall o || isRet o]
      cedges = concat [[(id, s) | s <- G.suc icfg id] | id <- cnodes]
      icfg'  = G.delEdges cedges icfg
      icfgr  = G.grev icfg'
      controlInsns = filter (\o -> isCall o || isTailCall o || isFun o || isRet o || isDelimiter o) (flatten code)
      cnodes' = filter (insertCandidate icfg controlInsns) cnodes
      creach = [(id, G.reachable id icfgr) | id <- cnodes']
      ymmreach = [snd (fromJust (G.lab icfg id)) | (id, reachers) <- creach,
                  any (isYMMDirtying icfg) reachers]
      code'  = foldl insertVzeroupper code ymmreach
  in f {fCode = code'}

isRet o =
  (TargetInstruction RETQ) `elem` oInstructions o

isYMMDirtying icfg id =
  isDirtyYMMOp $ snd (fromJust (G.lab icfg id))

branchInfo' bo @ SingleOperation {oOpr = Natural i}
  | isBranch bo = Just (branchInfo i)
  | otherwise = Nothing

insertVzeroupper code o =
  let (_, oid, _) = newIndexes $ flatten code
      vzu = mkLinear oid [TargetInstruction VZEROUPPER] [] []
      vzu' = mapToWrites (++ [OtherSideEffect YMM0]) vzu
      code' = map (insertOperationInBlock before (isIdOf o) vzu') code
   in code'

insertCandidate icfg controlInsns id =
  let o = snd (fromJust (G.lab icfg id))
  in unusedYMMAfter controlInsns o

unusedYMMAfter [] _ = True

unusedYMMAfter (call : (SingleOperation {oOpr = Virtual (Fun {oFunctionUs = funuses})}) : _) o
  | call == o
  = not (any ymmPreassigned funuses)

unusedYMMAfter (ret : out : _) o
  | ret == o
  = let (SingleOperation {oOpr = Virtual (Delimiter (Out {oOuts = outuses}))}) = out
    in not (any ymmPreassigned outuses)

unusedYMMAfter (_ : rest) o
  = unusedYMMAfter rest o

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
      srcedges = concat [[(p,id) | p <- G.pre icfg id] | id <- srcids, not (id `elem` sinkids)]
      icfg'  = G.delEdges srcedges icfg
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

