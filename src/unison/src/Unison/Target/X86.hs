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
module Unison.Target.X86 (target) where

import Debug.Trace
import Data.Maybe
import qualified Data.Set as S
import Control.Arrow

import Common.Util

import MachineIR
import MachineIR.Transformations.AddImplicitRegs

import Unison
import qualified Unison.Target.API as API
import Unison.Target.RegisterArray
import Unison.Target.Query
import Unison.Analysis.TemporaryType
import Unison.Target.X86.Common
import Unison.Target.X86.Registers
import Unison.Target.X86.OperandInfo
import Unison.Target.X86.Transforms
import Unison.Target.X86.Usages
import Unison.Target.X86.BranchInfo
import Unison.Target.X86.X86RegisterDecl
import Unison.Target.X86.X86ResourceDecl
import Unison.Target.X86.SpecsGen.X86InstructionDecl
import qualified Unison.Target.X86.SpecsGen as SpecsGen

target =
    API.TargetDescription {
      API.tRegisterArray    = const registerArray,
      API.tRegisterAtoms    = const registerAtoms,
      API.tRegClasses       = const regClasses,
      API.tRegisters        = const registers,
      API.tInfRegClassUsage = const infRegClassUsage,
      API.tInfRegClassBound = const infRegClassBound,
      API.tSubRegIndexType  = const subRegIndexType,
      API.tCallerSaved      = const callerSaved,
      API.tCalleeSaved      = const calleeSaved,
      API.tReserved         = const reserved,
      API.tInstructionType  = const instructionType,
      API.tBranchInfo       = const branchInfo,
      API.tPreProcess       = preProcess,
      API.tPostProcess      = postProcess,
      API.tTransforms       = const transforms,
      API.tCopies           = const copies,
      API.tRematInstrs      = const rematInstrs,
      API.tFromCopy         = const fromCopy,
      API.tOperandInfo      = const operandInfo,
      API.tAlignedPairs     = const SpecsGen.alignedPairs,
      API.tPackedPairs      = const (const (const [])),
      API.tRelatedPairs     = const (const []),
      API.tResources        = const resources,
      API.tUsages           = usages,
      API.tNop              = const nop,
      API.tReadWriteInfo    = const readWriteInfo,
      API.tImplementFrame   = const implementFrame,
      API.tAddPrologue      = const addPrologue,
      API.tAddEpilogue      = const addEpilogue,
      API.tStackDirection   = const stackDirection,
      API.tReadWriteLatency = readWriteLatency,
      API.tAlternativeTemps = const alternativeTemps,
      API.tExpandCopy       = const expandCopy,
      API.tConstraints      = const constraints
    }

instance Read X86Instruction where
  readsPrec _ strOp = [(SpecsGen.readOp strOp, "")]

-- | Gives the type of natural operation according to the instruction

instructionType i
    | i `elem` [TCRETURNdi, TCRETURNdi64,
                TCRETURNmi, TCRETURNmi64,
                TCRETURNri, TCRETURNri64,
                TAILJMPd, TAILJMPd64, TAILJMPd64_REX,
                TAILJMPm, TAILJMPm64, TAILJMPm64_REX,
                TAILJMPr, TAILJMPr64, TAILJMPr64_REX
                ] = TailCallInstructionType
    | otherwise = SpecsGen.instructionType i

-- | Gives a set of def copies and a list of sets of use copies to extend the
-- given temporary

-- Do not extend temporaries that are defined by virtual defines and used once
copies _fInfo False _t _rs d [_] | isDefine d = ([], [[]])

-- Do not extend temporaries that are only used by virtual kills
copies _fInfo False _t _rs _d [u] | isKill u = ([], [[]])

-- Do not extend temporaries pre-assigned to reserved registers
copies _ False _ rs _ us | any isReserved rs =
  ([], replicate (length us) [])

-- Add only one store for entry calle-saved temporaries
-- Add only one load for exit calle-saved temporaries
-- Do not add copies for intermediate calle-saved temporaries
copies (f, cst, _, _, _, _) False t [_r] _d [_u]
  | S.member t cst =
    (
      if isEntryTemp (fCode f) t
      then [mkNullInstruction, TargetInstruction PUSH_cst]
      else [],
      [if isExitTemp (fCode f) t
       then [mkNullInstruction, TargetInstruction POP_cst]
       else []]
    )

-- Do not extend non-pre-allocated temporaries that are only "passed through" a
-- block without calls
copies (f, _, _, _, _, _) False t [] d [u]
    | isIn d && isOut u && not (any isCall (bCode $ tempBlock (fCode f) t)) =
    ([], [[]])

-- Do not extend rematerializable instructions used only once, locally
-- FIXME: review whether this is always safe
copies (Function {fCode = code}, _, _, _, _, _) False t _ d [u]
  | isNatural d && (isNatural u || isFun u) &&
    (isRematerializable (targetInst (oInstructions d))) &&
    not (mayCrossMemDep readWriteInfo d u code) &&
    compatibleClassesForTemp t [d, u] = ([], [[]])

-- copies (f, _, cg, ra, bcfg, sg) _ t _rs d us =
--   let is     = d:us
--       w      = widthOfTemp ra cg f t is
--       dors   = transitivePreAssignments bcfg sg Reaching f t
--       uors   = transitivePreAssignments bcfg sg Reachable f t
--       size   = any ((==) Size) $ fGoal f
--   in (
--       (defCopies size w dors),
--       map (const (useCopies size w uors)) us
--       )
copies (f, _, cg, ra, _, _) _ t _ d us =
  let w = widthOfTemp ra cg f t (d:us)
  in
     (
       defCopies w,
       map (useCopies w) us
     )

defCopies 1 = [mkNullInstruction, TargetInstruction MOVE8, TargetInstruction STORE8]
defCopies 2 = [mkNullInstruction, TargetInstruction MOVE16, TargetInstruction STORE16]
defCopies 4 = [mkNullInstruction, TargetInstruction MOVE32, TargetInstruction STORE32]
defCopies 8 = [mkNullInstruction, TargetInstruction MOVE64, TargetInstruction STORE64]
defCopies 16 = [mkNullInstruction, TargetInstruction MOVE128, TargetInstruction STORE128]
defCopies 32 = [mkNullInstruction, TargetInstruction MOVE256, TargetInstruction STORE256]

useCopies 1 _ = [mkNullInstruction, TargetInstruction MOVE8, TargetInstruction LOAD8]
useCopies 2 _ = [mkNullInstruction, TargetInstruction MOVE16, TargetInstruction LOAD16]
useCopies 4 _ = [mkNullInstruction, TargetInstruction MOVE32, TargetInstruction LOAD32]
useCopies 8 _ = [mkNullInstruction, TargetInstruction MOVE64, TargetInstruction LOAD64]
useCopies 16 _ = [mkNullInstruction, TargetInstruction MOVE128, TargetInstruction LOAD128]
useCopies 32 _ = [mkNullInstruction, TargetInstruction MOVE256, TargetInstruction LOAD256]

classOfTemp = classOf (target, [])

widthOfTemp = widthOf (target, [])

compatibleClassesForTemp t os =
  let regs = [S.fromList $ registers $ fromJust (classOfTemp t o) | o <- os]
  in not $ S.null $ foldl S.intersection (head regs) regs

isReserved r = r `elem` reserved

-- NOTE: LEA is remat. if it depends on %rsp and no other reg
-- It will only be treated as a remat. candidate if that is the case
rematInstrs i
  | isRematerializable i =
      Just (sourceInstr i, dematInstr i, rematInstr i)
  | i `elem` [MOV8rm, MOV16rm, MOV32rm, MOV64rm,
              IMUL64rmi32,
              MOVSX16rm8, MOVSX32rm8, MOVSX32_NOREXrm8, MOVSX32rm16, MOVSX64rm8, MOVSX64rm16, MOVSX64rm32, 
              MOVZX16rm8, MOVZX32rm8, MOVZX32_NOREXrm8, MOVZX32rm16, MOVZX64rm8, MOVZX64rm16,
              VMOVAPSYrm,
              SETAEr, SETAr, SETBEr, SETBr, SETEr, SETGEr, SETGr, SETLEr, SETLr,
              SETNEr, SETNOr, SETNPr, SETNSr, SETOr, SETPr, SETSr,
              SETB_C8r, SETB_C16r, SETB_C32r, SETB_C64r] = Nothing
  | otherwise = trace ("consider rematInstrs " ++ show i) Nothing

-- | Transforms copy instructions into natural instructions

-- handle regular copies
fromCopy _ Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | i `elem` [PUSH_cst] =
    Linear {oIs = [TargetInstruction PUSH_fi],
            oUs = [mkBoundMachineFrameObject True i d, s],
            oDs = []}
  | i `elem` [POP_cst] =
    Linear {oIs = [TargetInstruction POP_fi],
            oUs = [mkBoundMachineFrameObject True i s],
            oDs = [d]}
  | i `elem` [MOVE8, MOVE16, MOVE32, MOVE64, MOVE128, MOVE256] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs = [s],
            oDs = [d]}
  | i `elem` [STORE8, STORE16, STORE32, STORE64, STORE128, STORE256] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs = [mkBoundMachineFrameObject False i d,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg,
                   s],
            oDs = []}
  | i `elem` [LOAD8, LOAD16, LOAD32, LOAD64, LOAD128, LOAD256] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs = [mkBoundMachineFrameObject False i s,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg],
            oDs = [d]}

-- handle rematerialization copies
fromCopy (Just (Linear {oUs = us}))
         Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | isDematInstr i =
    Linear {oIs = [mkNullInstruction], oUs = [s], oDs = [d]}
  | isRematInstr i =
    Linear {oIs = [TargetInstruction (originalInstr i)], oUs = us, oDs = [d]}
  | True =
    Linear {oIs = [TargetInstruction i], oUs = [s], oDs = [d]}

-- handle rematerialization sources
fromCopy _ (Natural o @ Linear {oIs = [TargetInstruction i]})
  | isSourceInstr i = o {oIs = [mkNullInstruction]}

fromCopy _ (Natural o) = o
fromCopy _ o = error ("unmatched pattern: fromCopy " ++ show o)

fromCopyInstr i
  | isJust (SpecsGen.parent i) = fromJust (SpecsGen.parent i)

mkBoundMachineFrameObject fixedSpill i (Register r) =
    let size = stackSize i
    in mkBound (mkMachineFrameObject (infRegPlace r) (Just size) size
                fixedSpill)

stackSize op
  | op `elem` [STORE8, LOAD8] = 1
  | op `elem` [STORE16, LOAD16] = 2
  | op `elem` [STORE32, LOAD32] = 4
  | op `elem` [STORE64, LOAD64, PUSH_cst, POP_cst] = 8
  | op `elem` [STORE128, LOAD128] = 16
  | op `elem` [STORE256, LOAD256] = 32

-- | Declares target architecture resources

resources =
    [

     -- Boundle width (times 16 bits): upper bound given by size of compound
     -- instructions to be expanded

     Resource BundleWidth 50,

     -- Resources as defined by X86ScheduleV6

     Resource Pipe 1

    ]

-- | No-operation instruction

nop = Linear [TargetInstruction NOOP] [] []

readWriteInfo i
  | i `elem` [SUBRSP_pseudo, ADDRSP_pseudo] =
      second (++ [ControlSideEffect,OtherSideEffect RSP]) $ SpecsGen.readWriteInfo i
  | otherwise = SpecsGen.readWriteInfo i

-- | Implementation of frame setup and destroy operations. All functions
-- observed so far have a reserved call frame (hasReservedCallFrame(MF)), which
-- means frame setup and destroy operations are just removed (see
-- X86FrameLowering.cpp ("eliminateCallFramePseudoInstr")).

implementFrame = const []

-- | Adds function prologue, see corresponding logic in X86FrameLowering.cpp
-- ("emitPrologue")

addPrologue _ code = code
addEpilogue _ code = code

-- | Direction in which the stack grows
stackDirection = API.StackGrowsDown

-- | Target dependent pre-processing functions

preProcess _ = [mapToMachineInstruction promoteImplicitOperands]

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
  let oif = operandInfo i
      ws  = filter (writesSideEffect i) regs
      firstImpDef = length (snd oif) - length ws
      (eds, mos1) = splitAt firstImpDef mos
      rs  = filter (readsSideEffect i) regs
      firstImpUse = length (fst oif) - length rs
      (eus, mos2) = splitAt firstImpUse mos1
  in eds ++ map mkMachineReg ws ++ eus ++ map mkMachineReg rs ++ mos2

-- | Target dependent post-processing functions

postProcess to = [expandPseudos to, flip addImplicitRegs (target, [])]

expandPseudos to = mapToMachineBlock (expandBlockPseudos (expandPseudo to))

expandPseudo _ (MachineSingle {msOpcode = MachineTargetOpc SUBRSP_pseudo, msOperands = [MachineImm 0]})
  = [[mkMachineSingle (MachineTargetOpc NOOP) [] []]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc SUBRSP_pseudo,
                                   msOperands = [off]}
  = let sp = mkMachineReg RSP
    in [[mi {msOpcode = mkMachineTargetOpc SUB64ri32,
             msOperands = [sp, sp, off]}]]

expandPseudo _ (MachineSingle {msOpcode = MachineTargetOpc ADDRSP_pseudo, msOperands = [MachineImm 0]})
  = [[mkMachineSingle (MachineTargetOpc NOOP) [] []]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc ADDRSP_pseudo,
                                   msOperands = [off]}
  = let sp = mkMachineReg RSP
    in [[mi {msOpcode = mkMachineTargetOpc ADD64ri32,
             msOperands = [sp, sp, off]}]]

expandPseudo _ (MachineSingle {msOpcode = MachineTargetOpc PUSH_fi, msOperands = [_, s]})
  = [[mkMachineSingle (MachineTargetOpc PUSH64r) [] [s]]]

expandPseudo _ (MachineSingle {msOpcode = MachineTargetOpc POP_fi, msOperands = [d, _]})
  = [[mkMachineSingle (MachineTargetOpc POP64r) [] [d]]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc MOV_FROM_SP,
                                   msOperands = [dst]}
  = let sp = mkMachineReg RSP
    in [[mi {msOpcode = mkMachineTargetOpc MOV64rr,
             msOperands = [dst, sp]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc MOV_TO_SP,
                                   msOperands = [src]}
  = let sp = mkMachineReg RSP
    in [[mi {msOpcode = mkMachineTargetOpc MOV64rr,
             msOperands = [sp, src]}]]

expandPseudo _ mi @ MachineSingle {msOpcode = MachineTargetOpc ALIGN_SP_32}
  = let sp = mkMachineReg RSP
        im32 = mkMachineImm (-32)
    in [[mi {msOpcode = mkMachineTargetOpc AND64ri8,
             msOperands = [sp, sp, im32]}]]

expandPseudo _ mi = [[mi]]

-- | Gives a list of function transformers

transforms ImportPreLift = [peephole extractReturnRegs, addPrologueEpilogue, addVzeroupper]
transforms ImportPostLift = [mapToOperation handlePromotedOperands]
transforms ImportPostCC = [liftReturnAddress]
transforms ExportPreOffs = [revertFixedFrame]
transforms ExportPreLow = [myLowerFrameIndices]
transforms AugmentPostRW = [mapToOperation stackIndexReadsSP]
transforms _ = []

-- | Latency of read-write dependencies

readWriteLatency _ _ (_, Read) (_, Write) = 0
readWriteLatency _ _ ((_, VirtualType (DelimiterType InType)), _) (_, _) = 1
readWriteLatency _ _ ((_, VirtualType FunType), _) (_, _) = 1
readWriteLatency _ _ ((_, VirtualType _), _) (_, _) = 0
readWriteLatency to _ ((TargetInstruction p, _), _) (_, _) =
    maybeMax 0 $ map occupation (usages to p)


-- | Alternative temporaries of each operand

-- All temps that hold the same value

alternativeTemps _ _ _ ts = map fst ts

-- | Copy expansion

expandCopy _ _ o = [o]

-- | Custom processor constraints

constraints _ = []

