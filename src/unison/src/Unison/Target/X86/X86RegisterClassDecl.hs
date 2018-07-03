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
    FPR |
    GR8 |
    GR8_NOREX |
    GR16 |
    GR16_AUX |
    GR32 |
    GR32_NOREX |
    GR32_NOAX |
    GR32_AUX |
    GR64 |
    GR64_NOSP |
    GR32orGR64 |
    GR128_AUX |
    Ptr_rc |
    Ptr_rc_nosp |
    Ptr_rc_norex |
    Ptr_rc_norex_nosp |
    Ptr_rc_tailcall |
    FR32 |
    FR64 |
    FR128 |
    FR128_AUX |
    VR128 |
    VR256 |
    VR2048_AUX |
    AMBIG |
    AUXE |
    AUXR |
    AUXB |
    M8 |
    M16 |
    M32 |
    M64 |
    M128 |
    M256 |
    RM8 |
    RM16 |
    RM32 |
    RM64 |
    RM128 |
    RM256
    deriving (Eq, Ord, Show, Read)
