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
module Unison.Tools.Export.LiftFixedFrameObjects (liftFixedFrameObjects) where

import Data.List
import qualified Data.Map as M

import Unison
import Unison.Analysis.FrameOffsets
import MachineIR

-- This pass lifts fixed spill frame objects (typically callee-saved registers
-- spilled with push and pop instructions), assigning them a slot.

liftFixedFrameObjects f @ Function {fCode = code, fFixedStackFrame = fobjs} _ =
  let mobjs  = nub $ concatMap fixedMachineFrameObjects $ flatten code
      fstIdx = newFrameIndex fobjs
      o2i    = M.fromList $ zip (map mfoOffset mobjs) [fstIdx..]
      code'  = mapToOperationInBlocks (toFrameIndexOperand o2i) code
      lfobjs = map (toFrameObject o2i) mobjs
      (_, lfobjs') = mapAccumL allocateObject (slotSet fobjs) lfobjs
  in f {fCode = code', fFixedStackFrame = fobjs ++ lfobjs'}

fixedMachineFrameObjects o =
  [mo | (Bound mo) <- oAllOps o, isMachineFrameObject mo, mfoFixedSpill mo]

toFrameIndexOperand = mapToOperandIf always . toFrameIndex

toFrameIndex o2i (Bound mfo) | isMachineFrameObject mfo && mfoFixedSpill mfo =
  let i = o2i M.! (mfoOffset mfo)
  in mkBound (mkMachineFrameIndex i True 0)
toFrameIndex _ op = op

toFrameObject o2i (MachineFrameObject off (Just size) align _) =
  let i = o2i M.! off
  in mkFrameObject i 0 (Just size) align Nothing
