module Unison.Target.X86.Usages (usages) where

import Data.List

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
  let us = itineraryUsage to i it
  in if unitLatency to then
       [u {occupation = 1, offset = 0} | u  <- us]
     else us

-- these are NoItinerary and disappear, whereas other NoItinerary are pseudos for real instructions
itineraryUsage to i it
  | isVoidInstruction i = []
  | skylake to =
      mergeUsages (skylakeUsage i it) [mkUsage Pipe 1 (pipeDuration i)]
  | otherwise = [mkUsage Pipe 1 (pipeDuration i)]

size i
  | isVoidInstruction i = 0
  | otherwise = 1

isVoidInstruction i
  | isSourceInstr i || isDematInstr i = True
  | i `elem`
      [SPILL32,
       SPILL,
       NOFPUSH,
       NOFPOP,
       BUNDLE,
       CATCHPAD,
       CATCHRET,
       CFI_INSTRUCTION,
       CS_PREFIX,
       DATA16_PREFIX,
       DBG_VALUE,
       DS_PREFIX,
       EH_LABEL,
       EH_RESTORE,
       ES_PREFIX,
       EXTRACT_SUBREG,
       FS_PREFIX,
       GC_LABEL,
       GS_PREFIX,
       IMPLICIT_DEF,
       INLINEASM,
       INSERT_SUBREG,
       Int_MemBarrier,
       Int_eh_sjlj_setup_dispatch,
       LIFETIME_END,
       LIFETIME_START,
       LOCAL_ESCAPE,
       LOCK_PREFIX,
       MONITOR,
       PATCHABLE_FUNCTION_ENTER,
       PATCHABLE_FUNCTION_EXIT,
       PHI,
       REPNE_PREFIX,
       REP_PREFIX,
       REX64_PREFIX,
       SEH_EndPrologue,
       SEH_Epilogue,
       SS_PREFIX,
       SUBREG_TO_REG,
       XACQUIRE_PREFIX,
       XRELEASE_PREFIX
       ] = True
  | otherwise = False

pipeDuration i
  | i == FPUSH32 = 3
  | otherwise = 1

-- skylakeUsage i it =
--   let original = case itProperties i it of
--                   Just (_, resources) -> resources
--                   Nothing -> []
--       expanded = concatMap expandResource original
--       combined = mergeAllUsages [mkUsage r 1 d | (r, d) <- expanded]
--   in combined

-- expandResource :: (X86Resource, Integer) -> [(X86Resource, Integer)]
-- expandResource (r, d) = nub [(r', d) | r' <- resourceHierarchy r]

-- For multiple uops, i:th uop gets offset i
skylakeUsage i it =
  let original = case itProperties i it of
                  Just (_, resources) -> resources
                  Nothing -> []
      original' = zip original [0..]
      expanded = concatMap expandResource original'
      combined = mergeAllUsages [(Usage r 1 d o) | ((r, d), o) <- expanded]
  in combined

mergeAllUsages usages = mergeUsages usages []

expandResource :: ((X86Resource, Integer), Integer) -> [((X86Resource, Integer), Integer)]
expandResource ((r, d), o) = nub [((r', d), o) | r' <- resourceHierarchy r]

resourceHierarchy :: X86Resource -> [X86Resource]
resourceHierarchy r = r : concatMap resourceHierarchy (superResources r)

superResources SKLPort0 = [SKLPort01, SKLPort05, SKLPort06]
superResources SKLPort1 = [SKLPort01, SKLPort15, SKLPort16]
superResources SKLPort4 = []
superResources SKLPort5 = [SKLPort05, SKLPort15]
superResources SKLPort6 = [SKLPort06, SKLPort16]
superResources SKLPort01 = [SKLPort015]
superResources SKLPort05 = [SKLPort015]
superResources SKLPort06 = [SKLPort0156]
superResources SKLPort15 = [SKLPort015]
superResources SKLPort16 = [SKLPort0156]
superResources SKLPort23 = [SKLPort237]
superResources SKLPort015 = [SKLPort0156]
superResources SKLPort237 = []
superResources SKLPort0156 = []
superResources SKLDivider = []
superResources SKLFPDivider = []
superResources ExePort = []
