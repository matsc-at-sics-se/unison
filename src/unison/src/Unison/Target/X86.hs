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
-- import Data.List
-- import Data.List.Split
import qualified Data.Set as S
-- import Control.Arrow

import Common.Util

import MachineIR
-- import MachineIR.Transformations.AddImplicitRegs

import Unison
import qualified Unison.Target.API as API
-- import Unison.Target.RegisterArray
import Unison.Target.Query
import Unison.Analysis.TemporaryType
-- import Unison.Transformations.FoldReservedRegisters
-- import Unison.Analysis.TransitiveOperations

import Unison.Target.X86.Common
import Unison.Target.X86.Registers
import Unison.Target.X86.OperandInfo
import Unison.Target.X86.Transforms
import Unison.Target.X86.Usages
-- import Unison.Target.X86.X86RegisterDecl
-- import Unison.Target.X86.X86RegisterClassDecl
import Unison.Target.X86.X86ResourceDecl
import Unison.Target.X86.SpecsGen.X86InstructionDecl
-- import Unison.Target.X86.SpecsGen.X86ItineraryDecl
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

-- | Gives the target of a jump instruction and the type of jump

branchInfo (Branch {oBranchIs = is, oBranchUs = (BlockRef l:_)})
  | targetInst is `elem` [JAE_1, JAE_2, JAE_4, JA_1, JA_2, JA_4, JBE_1, JBE_2, JBE_4,
     JB_1, JB_2, JB_4, JCXZ, JECXZ, JE_1, JE_2, JE_4, JGE_1, JGE_2,
     JGE_4, JG_1, JG_2, JG_4, JLE_1, JLE_2, JLE_4, JL_1, JL_2, JL_4,
     JNE_1, JNE_2, JNE_4, JNO_1, JNO_2, JNO_4, JNP_1, JNP_2,
     JNP_4, JNS_1, JNS_2, JNS_4, JO_1, JO_2, JO_4, JP_1, JP_2, JP_4,
     JRCXZ, JS_1, JS_2, JS_4, LOOP, LOOPE, LOOPNE] =
    BranchInfo Conditional (Just l)
  | targetInst is `elem` [JMP_1, JMP_2, JMP_4] =
    BranchInfo Unconditional (Just l)
branchInfo (Branch {oBranchIs = is})
  | targetInst is `elem` [JMP16m, JMP16r, JMP32m, JMP32r, JMP64m, JMP64r,
                          RETIL, RETIQ, RETIW, RETL, RETQ, RETW, LEAVE, LEAVE64] =
    BranchInfo Unconditional Nothing

branchInfo o = error ("unmatched pattern: branchInfo " ++ show (mkSingleOperation (-1) (Natural o)))

-- | Gives a set of def copies and a list of sets of use copies to extend the
-- given temporary

-- Do not extend temporaries that are defined by virtual defines and used once
copies _fInfo False _t _rs d [_] | isDefine d = ([], [[]])

-- Do not extend temporaries that are only used by virtual kills
copies _fInfo False _t _rs _d [u] | isKill u = ([], [[]])

-- Do not extend temporaries pre-assigned to reserved registers
copies _ False _ rs _ us | any isReserved rs =
  ([], replicate (length us) [])

-- Until we can spill to the stack frame, do not extend temporaries pre-assigned to callee-saved registers
copies (_, cst, _, _, _, _) False t rs _ [_] | not (null rs) && S.member t cst = ([], [[]])

-- FIXME:
-- -- Add only one store for entry callee-saved temporaries
-- -- Add only one load for exit callee-saved temporaries
-- -- Do not add copies for intermediate callee-saved temporaries
-- copies (f, cst, _, _, _, _) False t rs _ [_] | not (null rs) && S.member t cst =
--       (if isEntryTemp (fCode f) t
--        then [mkNullInstruction, TargetInstruction (pushInstruction rs)]
--        else [],
--        [if isExitTemp (fCode f) t
--         then [mkNullInstruction, TargetInstruction (popInstruction rs)]
--         else []])

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

defCopies 1 = [mkNullInstruction, TargetInstruction MOVE8]
defCopies 2 = [mkNullInstruction, TargetInstruction MOVE16]
defCopies 4 = [mkNullInstruction, TargetInstruction MOVE32]
defCopies 8 = [mkNullInstruction, TargetInstruction MOVE64]

useCopies 1 _ = [mkNullInstruction, TargetInstruction MOVE8]
useCopies 2 _ = [mkNullInstruction, TargetInstruction MOVE16]
useCopies 4 _ = [mkNullInstruction, TargetInstruction MOVE32]
useCopies 8 _ = [mkNullInstruction, TargetInstruction MOVE64]

-- FIXME: adapt for X86
-- pushInstruction [r]
  -- r == R4_7  = TPUSH_r4_7
  -- r == R8_11 = TPUSH_r8_11
  -- r == D8_15 = VSTMDDB_UPD_d8_15

-- popInstruction [r]
  -- r == R4_7  = TPOP_r4_7
  -- r == R8_11 = TPOP_r8_11
  -- r == D8_15 = VLDMDIA_UPD_d8_15

-- FIXME: adapt for X86
-- defCopies _ _ [Register (TargetRegister R7)] = []
-- defCopies size w _ =
--   [mkNullInstruction] ++
--    map TargetInstruction (moveInstrs size w) ++
--    map TargetInstruction (storeInstrs size w)

-- useCopies _ _ [Register (TargetRegister R7)] = []
-- useCopies size w _ =
--   [mkNullInstruction] ++
--    map TargetInstruction (moveInstrs size w) ++
--    map TargetInstruction (loadInstrs size w)

classOfTemp = classOf (target, [])

widthOfTemp = widthOf (target, [])

compatibleClassesForTemp t os =
  let regs = [S.fromList $ registers $ fromJust (classOfTemp t o) | o <- os]
  in not $ S.null $ foldl S.intersection (head regs) regs

-- TODO: add STORE_S, LOAD_S, also GPR <-> SPR moves?

-- FIXME: adapt for X86
-- -- TMOVr, {TMOVr, VMOVS, VMOVSR, VMOVRS} (MOVE_ALL is instanciated after
-- -- register allocation since their properties are all the same -- except TMOVr
-- -- has size 2)
-- moveInstrs size 1
--   | size = [MOVE, MOVE_ALL]
--   | otherwise = [MOVE_ALL]

-- -- VMOVD
-- moveInstrs _ 2 = [MOVE_D]

-- -- {T2STRi12, tSTRi}
-- storeInstrs size 1
--   | size = [STORE, STORE_T]
--   | otherwise = [STORE]

-- -- VSTRD
-- storeInstrs _ 2 = [STORE_D]

-- -- {T2LDRi12, tLDRi}
-- loadInstrs size 1
--   | size = [LOAD, LOAD_T]
--   | otherwise = [LOAD]

-- -- VLDRD
-- loadInstrs _ 2 = [LOAD_D]

isReserved r = r `elem` reserved

rematInstrs i
  | isRematerializable i =
      Just (sourceInstr i, dematInstr i, rematInstr i)
  | otherwise = trace ("consider rematInstrs " ++ show i) Nothing
  -- otherwise = error ("unmatched: rematInstrs " ++ show i)

-- | Transforms copy instructions into natural instructions

-- handle regular copies
fromCopy _ Copy {oCopyIs = [TargetInstruction i], oCopyS = s, oCopyD = d}
  | i `elem` [MOVE8, MOVE16, MOVE32, MOVE64] =
    Linear {oIs = [TargetInstruction (fromCopyInstr i)],
            oUs = [s],
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

-- mkPushRegs i = map (Register . TargetRegister) (pushRegs i)

-- mkOprX86SP = Register $ mkTargetRegister SP

-- mkBoundMachineFrameObject i (Register r) =
--     let size = stackSize i
--     in mkBound (mkMachineFrameObject (infRegPlace r) (Just size) size)

-- stackSize i
--   | i `elem` [STORE, STORE_T, LOAD, LOAD_T] = 1
--   | i `elem` [STORE_D, LOAD_D] = 2

fromCopyInstr i
  | isJust (SpecsGen.parent i) = fromJust (SpecsGen.parent i)

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

-- FIXME: adapt for X86
readWriteInfo i
--  -- copies do not have memory side effects (loads and stores do not alias
--  -- with other memory accesses as they operate on spill slots only)
--  | SpecsGen.instructionType i == CopyInstructionType = SpecsGen.readWriteInfo i
--  -- complete memory side effects (some mayLoad/mayStore info is missing)
--  | SpecsGen.itinerary i `elem`
--    [IIC_iLoad_m, IIC_iLoad_mu, IIC_iLoad_mBr, IIC_iLoad_bh_ru, IIC_iLoad_bh_iu,
--     IIC_iLoad_bh_r, IIC_iLoad_bh_si, IIC_iLoad_d_r, IIC_iLoad_d_ru,
--     IIC_iLoad_i, IIC_iLoadiALU, IIC_iLoad_ru, IIC_iLoad_iu, IIC_iLoad_r,
--     IIC_iLoad_si, IIC_fpLoad_mu, IIC_fpLoad_m, IIC_fpLoad64, IIC_fpLoad32,
--     IIC_iLoad_bh_i, IIC_iLoad_d_i] =
--      first addMem $ SpecsGen.readWriteInfo i
--  | SpecsGen.itinerary i `elem`
--    [IIC_iStore_r, IIC_iStore_bh_r, IIC_iStore_m, IIC_iStore_mu,
--     IIC_iStore_bh_ru, IIC_iStore_bh_iu, IIC_iStore_ru, IIC_iStore_bh_si,
--     IIC_iStore_d_r, IIC_iStore_d_ru, IIC_iStore_iu, IIC_iStore_si,
--     IIC_fpStore_mu, IIC_fpStore_m, IIC_fpStore64, IIC_fpStore32,
--     IIC_iStore_bh_i, IIC_iStore_i] =
--      second addMem $ SpecsGen.readWriteInfo i
--  | i `elem` [TSUBspi_pseudo, TADDspi_pseudo] =
--      second (++ [OtherSideEffect SP]) $ SpecsGen.readWriteInfo i
  | otherwise = SpecsGen.readWriteInfo i

-- addMem = (++ [Memory "mem"])

-- | Implementation of frame setup and destroy operations. All functions
-- observed so far have a reserved call frame (hasReservedCallFrame(MF)), which
-- means frame setup and destroy operations are just removed (see
-- X86FrameLowering.cpp ("eliminateCallFramePseudoInstr")).

implementFrame = const []

-- | Adds function prologue, see corresponding logic in X86FrameLowering.cpp
-- ("emitPrologue")

-- FIXME: adapt for X86

-- -- We need a stack frame iff there are 1) spills or 2) non-fixed stack objects
-- -- or SP-relative stores. This takes care of 1), the transformation
-- -- 'enforceStackFrame' at AugmentPostRW takes care of 2) which is a static
-- -- condition. Additionally, we need to adjust the SP by 1 if we have an odd
-- -- number of callee-saved spills for alignment reasons (see
-- -- http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.faqs/ka4127.html).
-- -- This is not yet supported, instead, 'uni normalize' removes SP adjustments in
-- -- such cases to ensure a fair comparison to LLVM.

-- addPrologue (_, oid, _) (e:code) =
--   let subSp = mkAct $ mkOpt oid TSUBspi_pseudo [Bound mkMachineFrameSize] []
--   in case split (keepDelimsR $ whenElt isFPPush) code of
--       [before, after] -> [e] ++ before ++ [subSp] ++ after
--       _ -> case split (dropInitBlank $ condense $ whenElt isStoreCopy) code of
--             before : after -> [e] ++ before ++ [subSp] ++ concat after

-- isFPPush o = TargetInstruction (VSTMDDB_UPD_d8_15) `elem` oInstructions o
-- isStoreCopy o = any (\i -> TargetInstruction i `elem` oInstructions o)
--                 [STORE, STORE_D, TPUSH2_r4_7, VSTMDDB_UPD_d8_15]

-- addEpilogue (_, oid, _) code =
--   let addSp = mkAct $ mkOpt oid TADDspi_pseudo [Bound mkMachineFrameSize] []
--   in case split (keepDelimsL $
--                  whenElt (\o -> isPopRet o || isBranch o || isTailCall o))
--           code of
--       f : e -> f ++ [addSp] ++ concat e
--       os    -> error ("unhandled epilogue: " ++ show os)

-- isPopRet o = any (\i -> TargetInstruction i `elem` oInstructions o)
--              [TPOP2_r4_7_RET, VLDMDIA_UPD_d8_15]

addPrologue _ os = os
addEpilogue _ os = os

-- mkOpt oid inst us ds =
--   makeOptional $ mkLinear oid [TargetInstruction inst] us ds

-- mkAct = addActivators (map TargetInstruction spillInstrs)

-- addActivators = mapToActivators . (++)

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

postProcess _ = []

-- pushRegs i
--   | i `elem` [TPUSH2_r4_7, TPOP2_r4_7, TPOP2_r4_7_RET] =
--       [R4, R5, R6, R7]
--   | i `elem` [TPUSH2_r4_11, TPOP2_r4_11, TPOP2_r4_11_RET] =
--       pushRegs TPUSH2_r4_7 ++ [R8, R9, R10, R11]
--   | i `elem` [VSTMDDB_UPD_d8_15, VLDMDIA_UPD_d8_15] =
--       [D8, D9, D10, D11, D12, D13, D14, D15]
-- pushRegs i = error ("unmatched: pushRegs " ++ show i)

-- | Gives a list of function transformers

transforms ImportPreLift = [peephole extractReturnRegs]
transforms ImportPostLift = [mapToOperation handlePromotedOperands,
                             mapToOperation handleStackOperands]
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

