module Unison.Target.X86.Usages (usages) where

import Unison
import Unison.Target.X86.Common
import Unison.Target.X86.X86ResourceDecl
import qualified Unison.Target.X86.SpecsGen as SpecsGen
import Unison.Target.X86.SpecsGen.X86ItineraryDecl
import Unison.Target.X86.SpecsGen.X86InstructionDecl

-- | Declares resource usages of each instruction

usages to i =
  let it = SpecsGen.itinerary i
  -- TODO: define instruction size as BundleWidth usage
  in mergeUsages (itineraryUsage' to i it)
     [mkUsage BundleWidth (size i) 1 | size i > 0]

itineraryUsage' to i it =
  let us = itineraryUsage i it
  in if unitLatency to then
       [u {occupation = 1, offset = 0} | u  <- us]
     else us

-- these are NoItinerary, but they are real instructions nevertheless AFAIK
itineraryUsage i _
  | i `elem`
      [BUNDLE, CATCHPAD, CATCHRET, CFI_INSTRUCTION, 
       CS_PREFIX, DATA16_PREFIX, DBG_VALUE, DS_PREFIX, EH_LABEL,
       EH_RESTORE, ES_PREFIX, EXTRACT_SUBREG, FAULTING_LOAD_OP, FS_PREFIX,
       GC_LABEL, GS_PREFIX, IMPLICIT_DEF, INLINEASM,
       INSERT_SUBREG,
       LEA16r_demat, LEA16r_source,
       LEA32r_demat, LEA32r_source,
       LEA64r_demat, LEA64r_source,
       LIFETIME_END, LIFETIME_START, LOCAL_ESCAPE, LOCK_PREFIX, MONITOR, 
       MOV16ri_alt_demat, MOV16ri_alt_source,
       MOV16ri_demat, MOV16ri_source,
       MOV32r0_demat, MOV32r0_source,
       MOV32r1_demat, MOV32r1_source,
       MOV32r_1_demat, MOV32r_1_source,
       MOV32ri64_demat, MOV32ri64_source,
       MOV32ri_alt_demat, MOV32ri_alt_source,
       MOV32ri_demat, MOV32ri_source,
       MOV64ri32_demat, MOV64ri32_source,
       MOV64ri_demat, MOV64ri_source,
       MOV8ri_demat, MOV8ri_source,
       PHI, REPNE_PREFIX, REP_PREFIX, REX64_PREFIX,
       SEH_EndPrologue, SEH_Epilogue,
       SS_PREFIX, SUBREG_TO_REG,
       V_SET0_demat, V_SET0_source,
       V_SETALLONES_demat, V_SETALLONES_source,
       AVX_SET0_demat, AVX_SET0_source,
       AVX2_SETALLONES_demat, AVX2_SETALLONES_source,
       FsFLD0SS_demat, FsFLD0SS_source,
       FsFLD0SD_demat, FsFLD0SD_source,
       XRELEASE_PREFIX, XACQUIRE_PREFIX]
  = []

itineraryUsage _ it
  | it `elem` [NoItinerary] = [mkUsage Pipe 1 1]

itineraryUsage _ _ = [mkUsage Pipe 1 1]

size _ = 1

