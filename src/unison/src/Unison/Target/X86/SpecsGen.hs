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
module Unison.Target.X86.SpecsGen (module X) where
  import Unison.Target.X86.SpecsGen.ReadWriteInfo as X
  import Unison.Target.X86.SpecsGen.OperandInfo as X
  import Unison.Target.X86.SpecsGen.ReadOp as X
  import Unison.Target.X86.SpecsGen.ShowInstance()
  import Unison.Target.X86.SpecsGen.Itinerary as X
  import Unison.Target.X86.SpecsGen.InstructionType as X
  import Unison.Target.X86.SpecsGen.AlignedPairs as X
  import Unison.Target.X86.SpecsGen.Size as X
  import Unison.Target.X86.SpecsGen.Parent as X
