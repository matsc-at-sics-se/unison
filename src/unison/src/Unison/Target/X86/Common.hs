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
module Unison.Target.X86.Common
    (unitLatency, align,
     isRematerializable, isSourceInstr,
     isDematInstr, isRematInstr, sourceInstr, dematInstr, rematInstr,
     originalInstr, spillInstrs,
     condMoveInstrs, condMoveInverses, condMoveAlts,
     promotedRegs, readsSideEffect,
     writesSideEffect, isDirtyYMMInsn, isDirtyYMMOp, isUseYMMInsn) where

import qualified Data.Map as M

import Unison
import qualified Unison.Target.API as API
import qualified Unison.Target.X86.SpecsGen as SpecsGen
import Unison.Target.X86.SpecsGen.X86InstructionDecl
import Unison.Target.X86.X86RegisterClassDecl
import Unison.Target.X86.X86RegisterDecl

unitLatency to = API.isBoolOption "unit-latency" to
align to = API.isBoolOption "align" to

data RematTriple = RematTriple {
  source :: X86Instruction,
  demat  :: X86Instruction,
  remat  :: X86Instruction}

isRematerializable i = M.member i rematVersions
isSourceInstr = isRInstrOf source
isDematInstr = isRInstrOf demat
isRematInstr = isRInstrOf remat
isRInstrOf f i = i `elem` [f t | t <- M.elems rematVersions]
sourceInstr i = source $ rematVersions M.! i
dematInstr  i = demat $ rematVersions M.! i
rematInstr  i = remat $ rematVersions M.! i

originalInstr i =
  (M.fromList [(remat ris, i) | (i, ris) <- M.toList rematVersions]) M.! i


rematVersions :: M.Map X86Instruction RematTriple
rematVersions = M.fromList
  [(MOV8ri, RematTriple MOV8ri_source MOV8ri_demat MOV8ri_remat),
   (MOV16ri, RematTriple MOV16ri_source MOV16ri_demat MOV16ri_remat),
   (MOV16ri_alt, RematTriple MOV16ri_alt_source MOV16ri_alt_demat MOV16ri_alt_remat),
   (MOV32r0, RematTriple MOV32r0_source MOV32r0_demat MOV32r0_remat),
   (MOV32r1, RematTriple MOV32r1_source MOV32r1_demat MOV32r1_remat),
   (MOV32r_1, RematTriple MOV32r_1_source MOV32r_1_demat MOV32r_1_remat),
   (MOV32ri, RematTriple MOV32ri_source MOV32ri_demat MOV32ri_remat),
   (MOV32ri64, RematTriple MOV32ri64_source MOV32ri64_demat MOV32ri64_remat),
   (MOV32ri_alt, RematTriple MOV32ri_alt_source MOV32ri_alt_demat MOV32ri_alt_remat),
   (MOV64ri, RematTriple MOV64ri_source MOV64ri_demat MOV64ri_remat),
   (MOV64ri32, RematTriple MOV64ri32_source MOV64ri32_demat MOV64ri32_remat),
   (LEA16r, RematTriple LEA16r_source LEA16r_demat LEA16r_remat),
   (LEA32r, RematTriple LEA32r_source LEA32r_demat LEA32r_remat),
   (LEA64r, RematTriple LEA64r_source LEA64r_demat LEA64r_remat),
   (V_SET0, RematTriple V_SET0_source V_SET0_demat V_SET0_remat),
   (V_SETALLONES, RematTriple V_SETALLONES_source V_SETALLONES_demat V_SETALLONES_remat),
   (AVX_SET0, RematTriple AVX_SET0_source AVX_SET0_demat AVX_SET0_remat),
   (AVX2_SETALLONES, RematTriple AVX2_SETALLONES_source AVX2_SETALLONES_demat AVX2_SETALLONES_remat),
   (FsFLD0SS, RematTriple FsFLD0SS_source FsFLD0SS_demat FsFLD0SS_remat),
   (FsFLD0SD, RematTriple FsFLD0SD_source FsFLD0SD_demat FsFLD0SD_remat)]

spillInstrs = [MOV8mr, MOV8mr_NOREX, MOV8rm, MOV8rm_NOREX,
               MOV16mr, MOV16rm, MOV32mr, MOV32rm, MOV64mr, MOV64rm]

condMoveAlts = M.fromList
  [(CMOVA16rr, CMOVA16rr_swap),
   (CMOVA32rr, CMOVA32rr_swap),
   (CMOVA64rr, CMOVA64rr_swap),
   (CMOVAE16rr, CMOVAE16rr_swap),
   (CMOVAE32rr, CMOVAE32rr_swap),
   (CMOVAE64rr, CMOVAE64rr_swap),
   (CMOVB16rr, CMOVB16rr_swap),
   (CMOVB32rr, CMOVB32rr_swap),
   (CMOVB64rr, CMOVB64rr_swap),
   (CMOVBE16rr, CMOVBE16rr_swap),
   (CMOVBE32rr, CMOVBE32rr_swap),
   (CMOVBE64rr, CMOVBE64rr_swap),
   (CMOVE16rr, CMOVE16rr_swap),
   (CMOVE32rr, CMOVE32rr_swap),
   (CMOVE64rr, CMOVE64rr_swap),
   (CMOVG16rr, CMOVG16rr_swap),
   (CMOVG32rr, CMOVG32rr_swap),
   (CMOVG64rr, CMOVG64rr_swap),
   (CMOVGE16rr, CMOVGE16rr_swap),
   (CMOVGE32rr, CMOVGE32rr_swap),
   (CMOVGE64rr, CMOVGE64rr_swap),
   (CMOVL16rr, CMOVL16rr_swap),
   (CMOVL32rr, CMOVL32rr_swap),
   (CMOVL64rr, CMOVL64rr_swap),
   (CMOVLE16rr, CMOVLE16rr_swap),
   (CMOVLE32rr, CMOVLE32rr_swap),
   (CMOVLE64rr, CMOVLE64rr_swap),
   (CMOVNE16rr, CMOVNE16rr_swap),
   (CMOVNE32rr, CMOVNE32rr_swap),
   (CMOVNE64rr, CMOVNE64rr_swap),
   (CMOVNO16rr, CMOVNO16rr_swap),
   (CMOVNO32rr, CMOVNO32rr_swap),
   (CMOVNO64rr, CMOVNO64rr_swap),
   (CMOVNP16rr, CMOVNP16rr_swap),
   (CMOVNP32rr, CMOVNP32rr_swap),
   (CMOVNP64rr, CMOVNP64rr_swap),
   (CMOVNS16rr, CMOVNS16rr_swap),
   (CMOVNS32rr, CMOVNS32rr_swap),
   (CMOVNS64rr, CMOVNS64rr_swap),
   (CMOVO16rr, CMOVO16rr_swap),
   (CMOVO32rr, CMOVO32rr_swap),
   (CMOVO64rr, CMOVO64rr_swap),
   (CMOVP16rr, CMOVP16rr_swap),
   (CMOVP32rr, CMOVP32rr_swap),
   (CMOVP64rr, CMOVP64rr_swap),
   (CMOVS16rr, CMOVS16rr_swap),
   (CMOVS32rr, CMOVS32rr_swap),
   (CMOVS64rr, CMOVS64rr_swap)]

condMoveForwards = map fst $ M.toList condMoveAlts

condMoveInverses = map snd $ M.toList condMoveAlts

condMoveMemories = [CMOVA16rm, CMOVA32rm, CMOVA64rm, CMOVAE16rm,
    CMOVAE32rm, CMOVAE64rm, CMOVB16rm, CMOVB32rm, CMOVB64rm,
    CMOVBE16rm, CMOVBE32rm, CMOVBE64rm, CMOVE16rm, CMOVE32rm,
    CMOVE64rm, CMOVG16rm, CMOVG32rm, CMOVG64rm, CMOVGE16rm,
    CMOVGE32rm, CMOVGE64rm, CMOVL16rm, CMOVL32rm, CMOVL64rm,
    CMOVLE16rm, CMOVLE32rm, CMOVLE64rm, CMOVNE16rm, CMOVNE32rm,
    CMOVNE64rm, CMOVNO16rm, CMOVNO32rm, CMOVNO64rm, CMOVNP16rm,
    CMOVNP32rm, CMOVNP64rm, CMOVNS16rm, CMOVNS32rm, CMOVNS64rm,
    CMOVO16rm, CMOVO32rm, CMOVO64rm, CMOVP16rm, CMOVP32rm, CMOVP64rm,
    CMOVS16rm, CMOVS32rm, CMOVS64rm]

condMoveInstrs =
  condMoveForwards ++ condMoveInverses ++ condMoveMemories

-- This list should contain exactly the registers that are promoted by
-- 'specsgen' (see 'run-specsgen-x86' recipe in Makefile).
promotedRegs = [AL, AH, AX, EAX, RAX, EBX, RBX, CL, CX, ECX, RCX, DX, EDX, RDX, EBP, RBP, EDI, RDI, ESI, RSI]

readsSideEffect i eff =
  (OtherSideEffect eff) `elem` (fst $ SpecsGen.readWriteInfo i)
writesSideEffect i eff =
  (OtherSideEffect eff) `elem` (snd $ SpecsGen.readWriteInfo i)

isDirtyYMMOp o
  = any isDirtyYMMInsn [(oTargetInstr oi) | oi <- (oInstructions o), isTargetInstruction oi]

isDirtyYMMInsn i
  = any temporaryInfoYMM (snd $ SpecsGen.operandInfo i)

isUseYMMInsn i
  = any temporaryInfoYMM (fst $ SpecsGen.operandInfo i)

temporaryInfoYMM TemporaryInfo {oiRegClass = (RegisterClass VR256)} = True
temporaryInfoYMM _ = False
