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
module Unison.Target.X86.X86RegisterClassDecl (X86RegisterClass (..)) where

data X86RegisterClass =
    ALL |
    CCR |
    GPR |
    GR8 |
    GR8_NOREX |
    GR16 |
    GR32 |
    GR32_NOREX |
    GR32_NOAX |
    GR64 |
    GR64_NOSP |
    Ptr_rc |
    Ptr_rc_nosp |
    Ptr_rc_norex |
    Ptr_rc_norex_nosp |
    Ptr_rc_tailcall |
    M8 |
    M16 |
    M32 |
    M64 |
    RM8 |
    RM16 |
    RM32 |
    RM64
    deriving (Eq, Ord, Show, Read)
