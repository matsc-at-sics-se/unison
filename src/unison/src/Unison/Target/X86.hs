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
import Data.List
import Data.Maybe
import qualified Data.Set as S

import Common.Util

import MachineIR

import Unison
import qualified Unison.Target.API as API
import Unison.Target.RegisterArray
import Unison.Target.Query
import Unison.Analysis.TemporaryType
import Unison.Analysis.TransitiveOperations
import Unison.Target.X86.Common
import Unison.Target.X86.Registers
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
      API.tOperandInfo      = operandInfo,
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

-- | Gives a set of def copies and a list of sets of use copies to extend the
-- given temporary

-- Do not extend temporaries that are defined by virtual defines and used once
copies _fInfo False _t _rs d [_] | isDefine d = ([], [[]])

-- Do not extend temporaries that are only used by virtual kills
copies _fInfo False _t _rs _d [u] | isKill u = ([], [[]])

-- Do not extend temporaries pre-assigned to reserved registers
copies _ False _ rs _ us | any isReserved rs =
  ([], replicate (length us) [])

-- Add only one store for entry callee-saved temporaries
-- Add only one load for exit callee-saved temporaries
-- Do not add copies for intermediate callee-saved temporaries
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
copies (Function {fCode = code}, _, _, _, _, _) False t _ d [u]
  | isNatural d && (isNatural u || isFun u) &&
    (isRematerializable (targetInst (oInstructions d))) &&
    not (mayCrossMemDep readWriteInfo d u code) &&
    compatibleClassesForTemp t [d, u] = ([], [[]])

-- If function has >6 arguments, then they will be accessed off RBX. Therefore,
-- do not spill def and use stemming from
--  o12: [t24:rbp] <- FPUSH32 [_]
--  o35: [] <- FPOP32 [t24,_]
copies (Function {fFixedStackFrame = fobjs}, _, _cg, _ra, _, _) _
       (Temporary {tReg = (Just (Register (TargetRegister RBX)))}) _ _d us
  | (TargetInstruction FPUSH32) `elem` (concatMap oInstructions us) &&
    (maximum $ map foOffset fobjs) >= 0
  = ([], [[]])

-- If 32-bit temp is used by some (combine), then prevent spilling,
-- because the upper 32 bits could be assumed to be zero later.
-- Otherwise, extend def and uses.
copies (f, _, cg, ra, bcfg, sg) _ t _ d us =
  let is = d:us
      w  = widthOfTemp ra cg f t is
      -- This below is just to distinguish floating-point from integer
      drcs = transitiveRegClasses (operandInfo []) bcfg sg Reaching f t
      dors = transitivePreAssignments bcfg sg Reaching f t
      urcs = transitiveRegClasses (operandInfo []) bcfg sg Reachable f t
      uors = transitivePreAssignments bcfg sg Reachable f t
      rcType rcs ors
        | null rcs && null ors = AnyRegClass
        | any isFloatClass rcs || any isFloatRegIR ors = FloatRegClass
        | otherwise = IntRegClass
      (drc, urc) = (rcType drcs dors, rcType urcs uors)
  in if w == 4 && all isCombine us
         then ([], [[]])
     else if w == 4 && any isCombine us
         then (defCopies drc w, [useCopies urc w | u <- us, not $ isCombine u])
         else (defCopies drc w, [useCopies urc w | _ <- us])

data RegClassType =
  AnyRegClass |
  IntRegClass |
  FloatRegClass
  deriving Show

defCopies IntRegClass 1 = [mkNullInstruction] ++ map TargetInstruction [IMOVE8, ISTORE8]
defCopies IntRegClass 2 = [mkNullInstruction] ++ map TargetInstruction [IMOVE16, ISTORE16]
defCopies IntRegClass 4 = [mkNullInstruction] ++ map TargetInstruction [IMOVE32, ISTORE32]
defCopies IntRegClass 8 = [mkNullInstruction] ++ map TargetInstruction [IMOVE64, ISTORE64]
defCopies FloatRegClass 4 = [mkNullInstruction] ++ map TargetInstruction [FMOVE32, FSTORE32]
defCopies FloatRegClass 8 = [mkNullInstruction] ++ map TargetInstruction [FMOVE64, FSTORE64]
defCopies FloatRegClass 16 = [mkNullInstruction] ++ map TargetInstruction [FMOVE128, FSTORE128]
defCopies FloatRegClass 32 = [mkNullInstruction] ++ map TargetInstruction [FMOVE256, FSTORE256]
defCopies AnyRegClass 1 = [mkNullInstruction] ++ map TargetInstruction [IMOVE8, ISTORE8]
defCopies AnyRegClass 2 = [mkNullInstruction] ++ map TargetInstruction [IMOVE16, ISTORE16]
defCopies AnyRegClass 4 = [mkNullInstruction] ++ map TargetInstruction [IMOVE32, ISTORE32, FMOVE32, FSTORE32]
defCopies AnyRegClass 8 = [mkNullInstruction] ++ map TargetInstruction [IMOVE64, ISTORE64, FMOVE64, FSTORE64]
defCopies AnyRegClass 16 = [mkNullInstruction] ++ map TargetInstruction [FMOVE128, FSTORE128]
defCopies AnyRegClass 32 = [mkNullInstruction] ++ map TargetInstruction [FMOVE256, FSTORE256]

useCopies IntRegClass 1 = [mkNullInstruction] ++ map TargetInstruction [IMOVE8, ILOAD8]
useCopies IntRegClass 2 = [mkNullInstruction] ++ map TargetInstruction [IMOVE16, ILOAD16]
useCopies IntRegClass 4 = [mkNullInstruction] ++ map TargetInstruction [IMOVE32, ILOAD32]
useCopies IntRegClass 8 = [mkNullInstruction] ++ map TargetInstruction [IMOVE64, ILOAD64]
useCopies FloatRegClass 4 = [mkNullInstruction] ++ map TargetInstruction [FMOVE32, FLOAD32]
useCopies FloatRegClass 8 = [mkNullInstruction] ++ map TargetInstruction [FMOVE64, FLOAD64]
useCopies FloatRegClass 16 = [mkNullInstruction] ++ map TargetInstruction [FMOVE128, FLOAD128]
useCopies FloatRegClass 32 = [mkNullInstruction] ++ map TargetInstruction [FMOVE256, FLOAD256]
useCopies AnyRegClass 1 = [mkNullInstruction] ++ map TargetInstruction [IMOVE8, ILOAD8]
useCopies AnyRegClass 2 = [mkNullInstruction] ++ map TargetInstruction [IMOVE16, ILOAD16]
useCopies AnyRegClass 4 = [mkNullInstruction] ++ map TargetInstruction [IMOVE32, ILOAD32, FMOVE32, FLOAD32]
useCopies AnyRegClass 8 = [mkNullInstruction] ++ map TargetInstruction [IMOVE64, ILOAD64, FMOVE64, FLOAD64]
useCopies AnyRegClass 16 = [mkNullInstruction] ++ map TargetInstruction [FMOVE128, FLOAD128]
useCopies AnyRegClass 32 = [mkNullInstruction] ++ map TargetInstruction [FMOVE256, FLOAD256]

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
  | i `elem` [FPUSH32, FPUSH, NOFPUSH,
              MOV8rm, MOV16rm, MOV32rm, MOV64rm,
              MOVSX16rm8, MOVSX32rm8, MOVSX32_NOREXrm8, MOVSX32rm16, MOVSX64rm8, MOVSX64rm16, MOVSX64rm32, 
              MOVZX16rm8, MOVZX32rm8, MOVZX32_NOREXrm8, MOVZX32rm16, MOVZX64rm8, MOVZX64rm16,
              IMUL64rmi32,
              VMOVSSrm, VMOVSDrm, VMOVAPSrm, VMOVUPSrm, VMOVAPSYrm, VMOVUPSYrm,
              SETAEr, SETAr, SETBEr, SETBr, SETEr, SETGEr, SETGr, SETLEr, SETLr,
              SETNEr, SETNOr, SETNPr, SETNSr, SETOr, SETPr, SETSr,
              SETB_C8r, SETB_C16r, SETB_C32r, SETB_C64r,
              MOV16mi_unison, MOV32mi_unison, MOV8mi_unison, MOV64mi32_unison,
              SETAEm_unison, SETAm_unison, SETBEm_unison, SETBm_unison,
              SETEm_unison, SETGEm_unison, SETGm_unison, SETLEm_unison,
              SETLm_unison, SETNEm_unison, SETNOm_unison, SETNPm_unison,
              SETNSm_unison, SETOm_unison, SETPm_unison, SETSm_unison] = Nothing
  | otherwise = trace ("uni: consider rematInstrs " ++ show i) Nothing

-- | Transforms copy instructions into natural instructions

--
-- handle regular copies
-- 

fromCopy _ Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | i `elem` [PUSH_cst] =
    Linear {oIs = [TargetInstruction PUSH_fi],
            oUs = [mkBoundMachineFrameObject True i d, s],
            oDs = []}
  | i `elem` [POP_cst] =
    Linear {oIs = [TargetInstruction POP_fi],
            oUs = [mkBoundMachineFrameObject True i s],
            oDs = [d]}
  | isMoveInstr i =
    Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [s],
            oDs = [d]}
  | isStoreInstr i =
    Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i d,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg,
                   s],
            oDs = []}
  | isLoadInstr i =
    Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i s,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg],
            oDs = [d]}

-- 
-- handle reg-mem variants of reg-reg instructions
-- 

fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [src1,src2], oDs = []})
  | isRegMemInstr i && nthUseIsInfinite 1 i
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i src1,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg,
                   src2],
            oDs = []}
fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [src1,src2], oDs = []})
  | isRegMemInstr i && nthUseIsInfinite 2 i
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [src1,
                   mkBoundMachineFrameObject False i src2,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg],
            oDs = []}
fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [src1], oDs = [dst]})
  | isRegMemInstr i && nthUseIsInfinite 1 i
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i src1,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg],
            oDs = [dst]}
fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [src1,src2], oDs = [dst]})
  | isRegMemInstr i && nthUseIsInfinite 1 i
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i src1,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg,
                   src2],
            oDs = [dst]}
fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [src1,src2], oDs = [dst]})
  | isRegMemInstr i && nthUseIsInfinite 2 i
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [src1,
                   mkBoundMachineFrameObject False i src2,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg],
            oDs = [dst]}
fromCopy _ o @ (Natural Linear {oIs = [TargetInstruction i]})
  | isRegMemInstr i
  = error ("unmatched pattern: fromCopy " ++ show o)

--
-- handle mem-reg variants of reg-reg instructions
--

fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [], oDs = [dst]})
  | isMemRegInstr i
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i dst,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg],
            oDs = []}
fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [src1], oDs = [src1']})
  | isMemRegInstr i && SpecsGen.alignedPairs i ([src1], [src1']) == [(src1,src1')]
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i src1',
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg],
            oDs = []}
fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [src1], oDs = [dst]})
  | isMemRegInstr i
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i dst,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg,
                   src1],
            oDs = []}
fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [src1], oDs = []})
  | isMemRegInstr i && nthUseIsInfinite 1 i
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i src1,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg],
            oDs = []}
fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [src1,src2], oDs = []})
  | isMemRegInstr i && nthUseIsInfinite 1 i
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i src1,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg,
                   src2],
            oDs = []}
fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [src1,src2], oDs = [src1']})
  | isMemRegInstr i && nthUseIsInfinite 1 i && SpecsGen.alignedPairs i ([src1,src2], [src1']) == [(src1,src1')]
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i src1,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg,
                   src2],
            oDs = []}
fromCopy _ (Natural Linear {oIs = [TargetInstruction i], oUs = [src1,src2,src3], oDs = [src2',src3']})
  | isMemRegInstr i && nthUseIsInfinite 1 i && src2 == src2' && src3 == src3'
  = Linear {oIs = [TargetInstruction (fromJust $ SpecsGen.parent i)],
            oUs = [mkBoundMachineFrameObject False i src1,
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg],
            oDs = [src2,src3]}
fromCopy _ o @ (Natural Linear {oIs = [TargetInstruction i]})
  | isMemRegInstr i
  = error ("unmatched pattern: fromCopy " ++ show o)

-- 
-- handle rematerialization copies
-- 

fromCopy (Just (Linear {oUs = us}))
         Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | isDematInstr i =
    Linear {oIs = [mkNullInstruction], oUs = [s], oDs = [d]}
  | isRematInstr i =
    fromCopy Nothing (Natural Linear {oIs = [TargetInstruction (originalInstr i)], oUs = us, oDs = [d]})
  | True =
    Linear {oIs = [TargetInstruction i], oUs = [s], oDs = [d]}

--
-- handle rematerialization sources
--

fromCopy _ (Natural o @ Linear {oIs = [TargetInstruction i]})
  | isSourceInstr i = o {oIs = [mkNullInstruction]}

-- 
-- handle reselected instructions
--

fromCopy _ (Natural Linear {oIs = [TargetInstruction MOV64ri64], oUs = us, oDs = [d]})
  = Linear {oIs = [TargetInstruction MOV32ri64], oUs = us, oDs = [reg64ToReg32 d]}
fromCopy _ (Natural Linear {oIs = [TargetInstruction MOV64r0], oUs = us, oDs = [d]})
  = Linear {oIs = [TargetInstruction MOV32r0], oUs = us, oDs = [reg64ToReg32 d]}
fromCopy _ (Natural Linear {oIs = [TargetInstruction ADD32ri_LEA], oUs = [r,i], oDs = [d]})
  = Linear {oIs = [TargetInstruction LEA64_32r],
            oUs = [mkBound (toMachineOperand (reg32ToReg64 r)),
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (toMachineOperand i),
                   mkBound MachineNullReg],
            oDs = [d]}
fromCopy _ (Natural Linear {oIs = [TargetInstruction ADD32rr_LEA], oUs = [r,r'], oDs = [d]})
  = Linear {oIs = [TargetInstruction LEA64_32r],
            oUs = [mkBound (toMachineOperand (reg32ToReg64 r)),
                   mkBound (mkMachineImm 1),
                   mkBound (toMachineOperand (reg32ToReg64 r')),
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg],
            oDs = [d]}
fromCopy _ (Natural Linear {oIs = [TargetInstruction ADD64ri_LEA], oUs = [r,i], oDs = [d]})
  = Linear {oIs = [TargetInstruction LEA64r],
            oUs = [mkBound (toMachineOperand r),
                   mkBound (mkMachineImm 1),
                   mkBound MachineNullReg,
                   mkBound (toMachineOperand i),
                   mkBound MachineNullReg],
            oDs = [d]}
fromCopy _ (Natural Linear {oIs = [TargetInstruction ADD64rr_LEA], oUs = [r,r'], oDs = [d]})
  = Linear {oIs = [TargetInstruction LEA64r],
            oUs = [mkBound (toMachineOperand r),
                   mkBound (mkMachineImm 1),
                   mkBound (toMachineOperand r'),
                   mkBound (mkMachineImm 0),
                   mkBound MachineNullReg],
            oDs = [d]}
fromCopy _ (Natural Linear {oIs = [TargetInstruction SHL32r1_LEA], oUs = [r], oDs = [d]})
  = Linear {oIs = [TargetInstruction LEA64_32r],
            oUs = [mkBound MachineNullReg,
                   mkBound (mkMachineImm 2),
                   mkBound (toMachineOperand (reg32ToReg64 r)),
                   mkBound (toMachineOperand 0),
                   mkBound MachineNullReg],
            oDs = [d]}
fromCopy _ (Natural Linear {oIs = [TargetInstruction SHL32ri_LEA], oUs = [r,i], oDs = [d]})
  = Linear {oIs = [TargetInstruction LEA64_32r],
            oUs = [mkBound MachineNullReg,
                   mkBound (mkMachineImm (logToMul i)),
                   mkBound (toMachineOperand (reg32ToReg64 r)),
                   mkBound (toMachineOperand 0),
                   mkBound MachineNullReg],
            oDs = [d]}
fromCopy _ (Natural Linear {oIs = [TargetInstruction SHL64r1_LEA], oUs = [r], oDs = [d]})
  = Linear {oIs = [TargetInstruction LEA64r],
            oUs = [mkBound MachineNullReg,
                   mkBound (mkMachineImm 2),
                   mkBound (toMachineOperand r),
                   mkBound (toMachineOperand 0),
                   mkBound MachineNullReg],
            oDs = [d]}
fromCopy _ (Natural Linear {oIs = [TargetInstruction SHL64ri_LEA], oUs = [r,i], oDs = [d]})
  = Linear {oIs = [TargetInstruction LEA64r],
            oUs = [mkBound MachineNullReg,
                   mkBound (mkMachineImm (logToMul i)),
                   mkBound (toMachineOperand r),
                   mkBound (toMachineOperand 0),
                   mkBound MachineNullReg],
            oDs = [d]}
fromCopy _ (Natural o @ Linear {oIs = [TargetInstruction ti], oUs = [src1,src2]})
  | ti `elem` condMoveInverses
  = o {oIs = [TargetInstruction (fromJust $ SpecsGen.parent ti)], oUs = [src2,src1]}

fromCopy _ (Natural o) = o
fromCopy _ o = error ("unmatched pattern: fromCopy " ++ show o)

logToMul (Bound (MachineImm 1)) = 2
logToMul (Bound (MachineImm 2)) = 4
logToMul (Bound (MachineImm 3)) = 8

mkBoundMachineFrameObject fixedSpill i (Register r) =
    let size = instrInfiniteUsage i
    in mkBound (mkMachineFrameObject (infRegPlace r) (Just size) size
                fixedSpill)

nthUseIsInfinite n i =
  let (use,_) = SpecsGen.operandInfo i
  in isInfiniteRegisterClass $ oiRegClass $ head $ drop (n-1) use

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

-- FIXME: revisit this clause if these instructions involve XOR
readWriteInfo i
  | i `elem` [MOV64r0, MOV64r0_remat, MOV32r0, MOV32r0_remat, MOV32r1, MOV32r1_remat, MOV32r_1, MOV32r_1_remat]
  = ([], [])

readWriteInfo i
  | isStoreInstr i || isLoadInstr i || isRegMemInstr i || isMemRegInstr i =
  let (rd, wr) = SpecsGen.readWriteInfo i
  in (delete (Memory "mem") rd, delete (Memory "mem") wr)

readWriteInfo i =
  SpecsGen.readWriteInfo i

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

preProcess _ = [disambiguateFunction,
                mkConsistentFunction,
                addRemat64bit,
                mapToMachineInstruction cleanFunRegisters,
                mapToMachineInstruction promoteImplicitOperands]

-- | Target dependent post-processing functions

postProcess to = [expandPseudos to,
                  mapToMachineInstruction demoteImplicitOperands,
                  mapToMachineInstruction addImplicitRegs,
                  mapToMachineInstruction ambiguateRegs]

-- | Gives a list of function transformers

transforms ImportPreLift = [peephole extractReturnRegs,
                            liftStackArgSize,
                            addPrologueEpilogue]
transforms ImportPostLift = [mapToOperation handlePromotedOperands,
                             generalizeRegisterDefines,
                             generalizeRegisterUses,
                             transAlternativeLEA]
transforms ImportPostCC = [liftReturnAddress]
transforms ExportPreOffs = [revertFixedFrame]
transforms ExportPreLow = [myLowerFrameIndices]
transforms AugmentPreRW = [suppressCombineCopies]
transforms AugmentPostRW = [movePrologueEpilogue,
                            addSpillIndicators,
                            addVzeroupper,
                            mapToOperation addStackIndexReadsSP,
                            mapToOperation addFunWrites,
                            removeDeadEflags
                            ]
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

-- force VZEROUPPER operations to be scheduled exactly one cycle before their
-- corresponding call/tailcall/return/out operation

constraints f =
  fixVzeroupperConstraints f ++
  prologueEpilogueConstraints f

fixVzeroupperConstraints f =
  let fcode = flatCode f
      ops = filter isVzeroupperRelevant fcode
  in fixVzeroupperConstraints' ops

fixVzeroupperConstraints' (o1 : o2 : rest)
  | isVzeroupper o1
  = [DistanceExpr (oId o2) (oId o1) (-1),
     DistanceExpr (oId o1) (oId o2) ( 1)] ++
    fixVzeroupperConstraints' rest
fixVzeroupperConstraints' (_ : rest)
  = fixVzeroupperConstraints' rest
fixVzeroupperConstraints' [] = []

isVzeroupperRelevant o =
  isVzeroupper o || isBranch o || isCall o || isTailCall o || isDelimiter o

isVzeroupper o =
  (TargetInstruction VZEROUPPER) `elem` oInstructions o

prologueEpilogueConstraints Function {fCode = blocks, fStackFrame = objs, fStackArgSize = sasize} =
  let fcode = flatten blocks
      align = maximum $ [0] ++ map foAlignment (objs)
      mustFpush32 = (align == 32)
      mustFpush = (align > 0 || sasize > 0)
      mayFpush = any isCall fcode
      blockInfo = concatMap proEpiInfo blocks
      blockIns = [oId $ blockIn b | b <- blocks, isEntryBlock b || isExitBlock b]
  in (concatMap indicatorConstraints $ zip blockIns blockInfo) ++
     (concatMap (prologueConstraints mustFpush32 mustFpush mayFpush) blockInfo) ++
     (concatMap (epilogueConstraints mustFpush32 mustFpush mayFpush) blockInfo)

indicatorConstraints (_, (Nothing, Nothing, _, _, _, _)) = []

indicatorConstraints (oin, (Nothing, Just osp, _, _, _, _))
  = [DistanceExpr osp   oin (-1)]

indicatorConstraints (oin, (Just osp32, Just osp, _, _, _, _))
  = [DistanceExpr osp32 oin (-1),
     DistanceExpr osp   oin (-1)]

prologueConstraints _ _ _ (_, _, Nothing, _, _, _) = []

prologueConstraints True _ _ (_, _, Just ofpush, _, _, _)
  = [ImplementsExpr ofpush (TargetInstruction FPUSH32)]

prologueConstraints _ True _ (Nothing, _, Just ofpush, _, _, _)
  = [ImplementsExpr ofpush (TargetInstruction FPUSH)]

prologueConstraints _ True _ (Just osp32, _, Just ofpush, _, _, _)
  = [XorExpr (ActiveExpr osp32) (ImplementsExpr ofpush (TargetInstruction FPUSH))] ++
    [NotExpr (ImplementsExpr ofpush (TargetInstruction NOFPUSH))]

prologueConstraints _ _ True (Nothing, Just _, Just ofpush, _, [], _)
  = [ImplementsExpr ofpush (TargetInstruction FPUSH)]

prologueConstraints _ _ True (Nothing, Just osp, Just ofpush, _, [p1,p2,p3,p4,p5,p6], _) =
  let nofcond = AndExpr [NotExpr (ActiveExpr osp),
                         XorExpr (XorExpr (XorExpr (ActiveExpr p1) (ActiveExpr p2))
                                          (XorExpr (ActiveExpr p3) (ActiveExpr p4)))
                                 (XorExpr (ActiveExpr p5) (ActiveExpr p6))]
  in [NotExpr (ImplementsExpr ofpush (TargetInstruction FPUSH32))] ++
     [XorExpr (NotExpr nofcond) (ImplementsExpr ofpush (TargetInstruction NOFPUSH))]

prologueConstraints _ _ True (Just osp32, Just _, Just ofpush, _, [], _)
  = [XorExpr (NotExpr (ActiveExpr osp32)) (ImplementsExpr ofpush (TargetInstruction FPUSH32))] ++
    [NotExpr (ImplementsExpr ofpush (TargetInstruction NOFPUSH))]

prologueConstraints _ _ True (Just osp32, Just osp, Just ofpush, _, [p1,p2,p3,p4,p5,p6], _) =
  let nofcond = AndExpr [NotExpr (ActiveExpr osp32),
                         NotExpr (ActiveExpr osp),
                         XorExpr (XorExpr (XorExpr (ActiveExpr p1) (ActiveExpr p2))
                                          (XorExpr (ActiveExpr p3) (ActiveExpr p4)))
                                 (XorExpr (ActiveExpr p5) (ActiveExpr p6))]
  in [XorExpr (NotExpr (ActiveExpr osp32)) (ImplementsExpr ofpush (TargetInstruction FPUSH32))] ++
     [XorExpr (NotExpr nofcond) (ImplementsExpr ofpush (TargetInstruction NOFPUSH))]

prologueConstraints _ _ _ (Nothing, Just osp, Just ofpush, _, _, _) =
  let fcond = ActiveExpr osp
  in [NotExpr (ImplementsExpr ofpush (TargetInstruction FPUSH32))] ++
     [XorExpr fcond (ImplementsExpr ofpush (TargetInstruction NOFPUSH))]

prologueConstraints _ _ _ (Just osp32, Just osp, Just ofpush, _, _, _) =
  let nofcond = AndExpr [NotExpr (ActiveExpr osp32),
                         NotExpr (ActiveExpr osp)]
  in [XorExpr (NotExpr (ActiveExpr osp32)) (ImplementsExpr ofpush (TargetInstruction FPUSH32))] ++
     [XorExpr (NotExpr nofcond) (ImplementsExpr ofpush (TargetInstruction NOFPUSH))]

epilogueConstraints _ _ _ (_, _, _, Nothing, _, _) = []

epilogueConstraints True _ _ (_, _, _, Just ofpop, _, _)
  = [ImplementsExpr ofpop (TargetInstruction FPOP32)]

epilogueConstraints _ True _ (Nothing, _, _, Just ofpop, _, _)
  = [ImplementsExpr ofpop (TargetInstruction FPOP)]

epilogueConstraints _ True _ (Just osp32, _, _, Just ofpop, _, _)
  = [XorExpr (ActiveExpr osp32) (ImplementsExpr ofpop (TargetInstruction FPOP))] ++
    [NotExpr (ImplementsExpr ofpop (TargetInstruction NOFPOP))]

epilogueConstraints _ _ True (Nothing, Just _, _, Just ofpop, _, [])
  = [ImplementsExpr ofpop (TargetInstruction FPOP)]

epilogueConstraints _ _ True (Nothing, Just osp, _, Just ofpop, _, [p1,p2,p3,p4,p5,p6]) =
  let nofcond = AndExpr [NotExpr (ActiveExpr osp),
                         XorExpr (XorExpr (XorExpr (ActiveExpr p1) (ActiveExpr p2))
                                          (XorExpr (ActiveExpr p3) (ActiveExpr p4)))
                                 (XorExpr (ActiveExpr p5) (ActiveExpr p6))]
  in [NotExpr (ImplementsExpr ofpop (TargetInstruction FPOP32))] ++
     [XorExpr (NotExpr nofcond) (ImplementsExpr ofpop (TargetInstruction NOFPOP))]

epilogueConstraints _ _ True (Just osp32, Just _, _, Just ofpop, _, [])
  = [XorExpr (NotExpr (ActiveExpr osp32)) (ImplementsExpr ofpop (TargetInstruction FPOP32))] ++
    [NotExpr (ImplementsExpr ofpop (TargetInstruction NOFPOP))]

epilogueConstraints _ _ True (Just osp32, Just osp, _, Just ofpop, _, [p1,p2,p3,p4,p5,p6]) =
  let nofcond = AndExpr [NotExpr (ActiveExpr osp32),
                         NotExpr (ActiveExpr osp),
                         XorExpr (XorExpr (XorExpr (ActiveExpr p1) (ActiveExpr p2))
                                          (XorExpr (ActiveExpr p3) (ActiveExpr p4)))
                                 (XorExpr (ActiveExpr p5) (ActiveExpr p6))]
  in [XorExpr (NotExpr (ActiveExpr osp32)) (ImplementsExpr ofpop (TargetInstruction FPOP32))] ++
     [XorExpr (NotExpr nofcond) (ImplementsExpr ofpop (TargetInstruction NOFPOP))]

epilogueConstraints _ _ _ (Nothing, Just osp, _, Just ofpop, _, _) =
  let fcond = ActiveExpr osp
  in [NotExpr (ImplementsExpr ofpop (TargetInstruction FPOP32))] ++
     [XorExpr fcond (ImplementsExpr ofpop (TargetInstruction NOFPOP))]

epilogueConstraints _ _ _ (Just osp32, Just osp, _, Just ofpop, _, _) =
  let nofcond = AndExpr [NotExpr (ActiveExpr osp32),
                         NotExpr (ActiveExpr osp)]
  in [XorExpr (NotExpr (ActiveExpr osp32)) (ImplementsExpr ofpop (TargetInstruction FPOP32))] ++
     [XorExpr (NotExpr nofcond) (ImplementsExpr ofpop (TargetInstruction NOFPOP))]

proEpiInfo b @ Block {bCode = code}
  | isEntryBlock b || isExitBlock b
  = [proEpiInfo' Nothing Nothing Nothing Nothing [] [] code]

proEpiInfo _ = []

proEpiInfo' osp32 osp ofpush ofpop opushl opopl []
  = (osp32, osp, ofpush, ofpop, opushl, opopl)

proEpiInfo' _ osp ofpush ofpop opushl opopl (o : code)
  | (TargetInstruction SPILL32) `elem` oInstructions o
  = proEpiInfo' (Just (oId o)) osp ofpush ofpop opushl opopl code

proEpiInfo' osp32 _ ofpush ofpop opushl opopl (o : code)
  | (TargetInstruction SPILL) `elem` oInstructions o
  = proEpiInfo' osp32 (Just (oId o)) ofpush ofpop opushl opopl code

proEpiInfo' osp32 osp _ ofpop opushl opopl (o : code)
  | (TargetInstruction FPUSH) `elem` oInstructions o
  = proEpiInfo' osp32 osp (Just (oId o)) ofpop opushl opopl code

proEpiInfo' osp32 osp ofpush _ opushl opopl (o : code)
  | (TargetInstruction FPOP) `elem` oInstructions o
  = proEpiInfo' osp32 osp ofpush (Just (oId o)) opushl opopl code

proEpiInfo' osp32 osp ofpush ofpop opushl opopl (o : code)
  | (TargetInstruction PUSH_cst) `elem` oInstructions o
  = proEpiInfo' osp32 osp ofpush ofpop ((oId o) : opushl) opopl code

proEpiInfo' osp32 osp ofpush ofpop opushl opopl (o : code)
  | (TargetInstruction POP_cst) `elem` oInstructions o
  = proEpiInfo' osp32 osp ofpush ofpop opushl ((oId o) : opopl) code

proEpiInfo' osp32 osp ofpush ofpop opushl opopl (_ : code)
  = proEpiInfo' osp32 osp ofpush ofpop opushl opopl code

-- slight adjustment of operandInfo

operandInfo _ i
  | i `elem` [NOFPUSH] =
    let ([use], [def]) = SpecsGen.operandInfo i
    in ([use], [def {oiLatency = 0}])
  | otherwise =
    SpecsGen.operandInfo i

