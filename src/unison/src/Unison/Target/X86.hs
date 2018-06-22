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

import Common.Util

import MachineIR

import Unison
import qualified Unison.Target.API as API
import Unison.Target.RegisterArray
import Unison.Target.Query
import Unison.Analysis.TemporaryType
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
copies (f, _, cg, ra, _, _) _ t _ d us =
  let w = widthOfTemp ra cg f t (d:us)
  in if w == 4 && all isCombine us
     then ([], [[]])
  else if w == 4 && any isCombine us
     then (defCopies w, [useCopies 4 u | u <- us, not $ isCombine u])
     else (defCopies w, map (useCopies w) us)

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
  | i `elem` [FPUSH32, FPUSH, NOFPUSH,
              MOV8rm, MOV16rm, MOV32rm, MOV64rm,
              IMUL64rmi32,
              MOVSX16rm8, MOVSX32rm8, MOVSX32_NOREXrm8, MOVSX32rm16, MOVSX64rm8, MOVSX64rm16, MOVSX64rm32, 
              MOVZX16rm8, MOVZX32rm8, MOVZX32_NOREXrm8, MOVZX32rm16, MOVZX64rm8, MOVZX64rm16,
              VMOVAPSrm, VMOVUPSrm, VMOVSDrm, VMOVAPSYrm,
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

-- handle reselected instructions
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
  = o {oIs = [TargetInstruction (fromCopyInstr ti)], oUs = [src2,src1]}

fromCopy _ (Natural o) = o
fromCopy _ o = error ("unmatched pattern: fromCopy " ++ show o)

fromCopyInstr i
  | isJust (SpecsGen.parent i) = fromJust (SpecsGen.parent i)

logToMul (Bound (MachineImm 1)) = 2
logToMul (Bound (MachineImm 2)) = 4
logToMul (Bound (MachineImm 3)) = 8

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

-- FIXME: revisit this clause if these instructions involve XOR
readWriteInfo i
  | i `elem` [MOV32r0, MOV32r0_remat, MOV32r1, MOV32r1_remat, MOV32r_1, MOV32r_1_remat]
  = ([], [])

readWriteInfo i
  = SpecsGen.readWriteInfo i

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

preProcess _ = [mapToMachineInstruction cleanFunRegisters,
                mapToMachineInstruction promoteImplicitOperands]

-- | Target dependent post-processing functions

postProcess to = [expandPseudos to,
                  mapToMachineInstruction demoteImplicitOperands,
                  mapToMachineInstruction addImplicitRegs]

-- | Gives a list of function transformers

transforms ImportPreLift = [peephole extractReturnRegs,
                            liftStackArgSize,
                            addPrologueEpilogue]
transforms ImportPostLift = [mapToOperation handlePromotedOperands,
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
                            -- moveOptWritesEflags
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
      blockIns = map (oId . blockIn) blocks
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

prologueConstraints _ _ True (Nothing, Just osp, Just ofpush, _, [p1,p2,p3,p4,p5,p6], _) =
  let nofcond = AndExpr [NotExpr (ActiveExpr osp),
                         XorExpr (XorExpr (XorExpr (ActiveExpr p1) (ActiveExpr p2))
                                          (XorExpr (ActiveExpr p3) (ActiveExpr p4)))
                                 (XorExpr (ActiveExpr p5) (ActiveExpr p6))]
  in [NotExpr (ImplementsExpr ofpush (TargetInstruction FPUSH32))] ++
     [XorExpr (NotExpr nofcond) (ImplementsExpr ofpush (TargetInstruction NOFPUSH))]

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

epilogueConstraints _ _ True (Nothing, Just osp, _, Just ofpop, _, [p1,p2,p3,p4,p5,p6]) =
  let nofcond = AndExpr [NotExpr (ActiveExpr osp),
                         XorExpr (XorExpr (XorExpr (ActiveExpr p1) (ActiveExpr p2))
                                          (XorExpr (ActiveExpr p3) (ActiveExpr p4)))
                                 (XorExpr (ActiveExpr p5) (ActiveExpr p6))]
  in [NotExpr (ImplementsExpr ofpop (TargetInstruction FPOP32))] ++
     [XorExpr (NotExpr nofcond) (ImplementsExpr ofpop (TargetInstruction NOFPOP))]

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

