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
module Unison.Target.X86.Transforms
    (extractReturnRegs) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word
import Data.Bits
import Data.List
import Data.Maybe

import Common.Util
import Unison
import MachineIR
import Unison.Target.Query
import qualified Unison.Graphs.CG as CG
import qualified Unison.Graphs.Partition as P
import Unison.Target.X86.Common
import Unison.Target.X86.OperandInfo
import Unison.Target.X86.Usages
import Unison.Target.X86.X86ResourceDecl
import Unison.Target.X86.X86RegisterDecl
import Unison.Target.X86.SpecsGen.X86InstructionDecl
import Unison.Target.X86.Registers()

extractReturnRegs _ (
  c
  :
  o @ SingleOperation {oOpr = Virtual
                               (Delimiter oi @ (Out {oOuts = outs}))}
  :
  rest) _ | isTailCall c && all isRegister outs =
   (
    rest,
    [c,
     o {oOpr = Virtual (Delimiter oi {oOuts = []})}]
   )

extractReturnRegs _ (o : rest) _ = (rest, [o])

