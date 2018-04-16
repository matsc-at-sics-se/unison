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
      [ADD16ri8_DB, ADD16ri_DB, ADD16rr_DB, ADD32ri8_DB, ADD32ri_DB, ADD32rr_DB,
       ADD64ri32_DB, ADD64ri8_DB, ADD64rr_DB,
       ANDNPDrm, ANDNPDrr, ANDNPSrm, ANDNPSrr, ANDPDrm,
       ANDPDrr, ANDPSrm, ANDPSrr, BTR64rr, FsFLD0SD, FsFLD0SS, MOV16ao64,
       MOV16o64a, 
       MOV32ao64, MOV32o64a, 
       MOV32r1, MOV32r1_remat,
       MOV32r_1, MOV32r_1_remat,
       MOV32ri64, MOV32ri64_remat,
       MOV64ao64, MOV64o64a,
       MOV8ao64, MOV8o64a, MOVNTDQArm,
       MOVNTSD, MOVNTSS, ORPDrm, ORPDrr, ORPSrm, ORPSrr, PCMPEQQrm,
       PCMPEQQrr, PMULDQrm, PMULDQrr, PMULUDQrm, PMULUDQrr,
       RORX32mi, RORX32ri, RORX64mi, RORX64ri,
       ROUNDSDm, ROUNDSDr, ROUNDSSm, ROUNDSSr, SARX32rm, SARX32rr,
       SARX64rm, SARX64rr, SETB_C16r, SETB_C32r, SETB_C64r, SETB_C8r,
       SHLX32rm, SHLX32rr, SHLX64rm, SHLX64rr, SHRX32rm, SHRX32rr,
       SHRX64rm, SHRX64rr, TCRETURNdi, TCRETURNdi64, TCRETURNmi,
       TCRETURNmi64, TCRETURNri, TCRETURNri64, UD2B,
       V_SET0, V_SET0_remat, V_SETALLONES, V_SETALLONES_remat,
       XORPDrm, XORPDrr, XORPSrm, XORPSrr]
  = [mkUsage Pipe 1 1]

itineraryUsage _ it
  | it `elem` [NoItinerary] = []

itineraryUsage _ _ = [mkUsage Pipe 1 1]

size _ = 1

