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
     originalInstr, spillInstrs, condMoveInstrs, promotedRegs, readsSideEffect,
     writesSideEffect) where

import qualified Data.Map as M
import Data.Tuple

import MachineIR
import Unison
import qualified Unison.Target.API as API
import qualified Unison.Target.X86.SpecsGen as SpecsGen
import Unison.Target.X86.SpecsGen.X86InstructionDecl
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
  [(MOV16ri, RematTriple MOV16ri_source MOV16ri_demat MOV16ri_remat),
   (MOV16ri_alt, RematTriple MOV16ri_alt_source MOV16ri_alt_demat MOV16ri_alt_remat),
   (MOV32r0, RematTriple MOV32r0_source MOV32r0_demat MOV32r0_remat),
   (MOV32r1, RematTriple MOV32r1_source MOV32r1_demat MOV32r1_remat),
   (MOV32r_1, RematTriple MOV32r_1_source MOV32r_1_demat MOV32r_1_remat),
   (MOV32ri, RematTriple MOV32ri_source MOV32ri_demat MOV32ri_remat),
   (MOV32ri64, RematTriple MOV32ri64_source MOV32ri64_demat MOV32ri64_remat),
   (MOV32ri_alt, RematTriple MOV32ri_alt_source MOV32ri_alt_demat MOV32ri_alt_remat),
   (MOV64ri, RematTriple MOV64ri_source MOV64ri_demat MOV64ri_remat),
   (MOV64ri32, RematTriple MOV64ri32_source MOV64ri32_demat MOV64ri32_remat),
   (MOV8ri, RematTriple MOV8ri_source MOV8ri_demat MOV8ri_remat)]

spillInstrs = [MOV8mr, MOV8mr_NOREX, MOV8rm, MOV8rm_NOREX,
               MOV16mr, MOV16rm, MOV32mr, MOV32rm, MOV64mr, MOV64rm]

condMoveInstrs = [CMOVA16rm, CMOVA16rr,
     CMOVA32rm, CMOVA32rr, CMOVA64rm, CMOVA64rr, CMOVAE16rm, CMOVAE16rr,
     CMOVAE32rm, CMOVAE32rr, CMOVAE64rm, CMOVAE64rr, CMOVB16rm,
     CMOVB16rr, CMOVB32rm, CMOVB32rr, CMOVB64rm, CMOVB64rr, CMOVBE16rm,
     CMOVBE16rr, CMOVBE32rm, CMOVBE32rr, CMOVBE64rm, CMOVBE64rr,
     CMOVE16rm, CMOVE16rr, CMOVE32rm, CMOVE32rr, CMOVE64rm, CMOVE64rr,
     CMOVG16rm, CMOVG16rr, CMOVG32rm, CMOVG32rr, CMOVG64rm, CMOVG64rr,
     CMOVGE16rm, CMOVGE16rr, CMOVGE32rm, CMOVGE32rr, CMOVGE64rm,
     CMOVGE64rr, CMOVL16rm, CMOVL16rr, CMOVL32rm, CMOVL32rr, CMOVL64rm,
     CMOVL64rr, CMOVLE16rm, CMOVLE16rr, CMOVLE32rm, CMOVLE32rr,
     CMOVLE64rm, CMOVLE64rr, CMOVNE16rm, CMOVNE16rr, CMOVNE32rm,
     CMOVNE32rr, CMOVNE64rm, CMOVNE64rr, CMOVNO16rm, CMOVNO16rr,
     CMOVNO32rm, CMOVNO32rr, CMOVNO64rm, CMOVNO64rr, CMOVNP16rm,
     CMOVNP16rr, CMOVNP32rm, CMOVNP32rr, CMOVNP64rm, CMOVNP64rr,
     CMOVNS16rm, CMOVNS16rr, CMOVNS32rm, CMOVNS32rr, CMOVNS64rm,
     CMOVNS64rr, CMOVO16rm, CMOVO16rr, CMOVO32rm, CMOVO32rr, CMOVO64rm,
     CMOVO64rr, CMOVP16rm, CMOVP16rr, CMOVP32rm, CMOVP32rr, CMOVP64rm,
     CMOVP64rr, CMOVS16rm, CMOVS16rr, CMOVS32rm, CMOVS32rr, CMOVS64rm,
     CMOVS64rr]

-- This list should contain exactly the registers that are promoted by
-- 'specsgen' (see 'run-specsgen-x86' recipe in Makefile).
promotedRegs = [CL, RAX, RDX]

readsSideEffect i eff =
  (OtherSideEffect eff) `elem` (fst $ SpecsGen.readWriteInfo i)
writesSideEffect i eff =
  (OtherSideEffect eff) `elem` (snd $ SpecsGen.readWriteInfo i)
