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
module Unison.Tools.Export.DirectFrame (directFrame) where

import Unison
import Unison.Target.API
import Unison.Analysis.FrameOffsets

-- This pass directs the frame (whether the stack grows up or down) as specified
-- by the target.

directFrame f @ Function {fFixedStackFrame = fobjs, fStackFrame = objs} target =
  case stackDirection target of
    StackGrowsDown ->
      let fobjs' = map revertDirection fobjs
          objs'  = map revertDirection objs
      in f {fFixedStackFrame = fobjs', fStackFrame = objs'}
    StackGrowsUp -> f
