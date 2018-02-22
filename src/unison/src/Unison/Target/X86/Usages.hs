module Unison.Target.X86.Usages (usages) where

import Data.List

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

itineraryUsage _ it
  | it `elem` [NoItinerary] = []

itineraryUsage _ _ = [mkUsage Pipe 1 1]

size _ = 1

