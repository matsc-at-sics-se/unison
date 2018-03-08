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
module Unison.Target.X86.OperandInfo (operandInfo) where

import qualified Unison.Target.X86.SpecsGen as SpecsGen

-- | Gives information about the operands of each instruction

operandInfo i = SpecsGen.operandInfo i
