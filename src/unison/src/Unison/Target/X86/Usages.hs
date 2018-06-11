module Unison.Target.X86.Usages (usages) where

import Unison
import Unison.Target.X86.Common
import Unison.Target.X86.X86ResourceDecl
import qualified Unison.Target.X86.SpecsGen as SpecsGen
import Unison.Target.X86.SpecsGen.X86InstructionDecl

-- | Declares resource usages of each instruction

usages to i =
  let it = SpecsGen.itinerary i
  -- TODO: define instruction size as BundleWidth usage
      uss = mergeUsages (itineraryUsage' to i it)
            [mkUsage BundleWidth (size i) 1 | size i > 0]
  in uss

itineraryUsage' to i it =
  let us = itineraryUsage i it
  in if unitLatency to then
       [u {occupation = 1, offset = 0} | u  <- us]
     else us

-- these are NoItinerary and disappear, whereas other NoItinerary are pseudos for real instructions
itineraryUsage i _
  | isSourceInstr i || isDematInstr i = []
  | i `elem`
      [BUNDLE, CATCHPAD, CATCHRET, CFI_INSTRUCTION, 
       CS_PREFIX, DATA16_PREFIX, DBG_VALUE, DS_PREFIX, EH_LABEL,
       EH_RESTORE, ES_PREFIX, EXTRACT_SUBREG, FAULTING_LOAD_OP, FS_PREFIX,
       GC_LABEL, GS_PREFIX, IMPLICIT_DEF, INLINEASM,
       INSERT_SUBREG,
       LIFETIME_END, LIFETIME_START, LOCAL_ESCAPE, LOCK_PREFIX, MONITOR, 
       PHI, REPNE_PREFIX, REP_PREFIX, REX64_PREFIX,
       SEH_EndPrologue, SEH_Epilogue,
       SS_PREFIX, SUBREG_TO_REG,
       XRELEASE_PREFIX, XACQUIRE_PREFIX] = []

itineraryUsage _ _ = [mkUsage Pipe 1 1]

size i
  | isSourceInstr i || isDematInstr i = 0
size _ = 1

