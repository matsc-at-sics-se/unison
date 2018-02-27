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
module Unison.Target.X86.X86RegisterDecl (X86Register (..)) where

data X86Register =
    EFLAGS |
    RIP |
    R000 |
    R001 |
    R002 |
    R003 |
    R004 |
    R005 |
    R006 |
    R007 | 
    R010 |
    R011 |
    R012 |
    R013 |
    R014 |
    R015 |
    R016 |
    R017 | 
    R020 |
    R021 |
    R022 |
    R023 |
    R024 |
    R025 |
    R026 |
    R027 | 
    R030 |
    R031 |
    R032 |
    R033 |
    R034 |
    R035 |
    R036 |
    R037 | 
    R040 |
    R041 |
    R042 |
    R043 |
    R044 |
    R045 |
    R046 |
    R047 | 
    R050 |
    R051 |
    R052 |
    R053 |
    R054 |
    R055 |
    R056 |
    R057 | 
    R060 |
    R061 |
    R062 |
    R063 |
    R064 |
    R065 |
    R066 |
    R067 | 
    R070 |
    R071 |
    R072 |
    R073 |
    R074 |
    R075 |
    R076 |
    R077 | 
    R100 |
    R101 |
    R102 |
    R103 |
    R104 |
    R105 |
    R106 |
    R107 | 
    R110 |
    R111 |
    R112 |
    R113 |
    R114 |
    R115 |
    R116 |
    R117 | 
    R120 |
    R121 |
    R122 |
    R123 |
    R124 |
    R125 |
    R126 |
    R127 | 
    R130 |
    R131 |
    R132 |
    R133 |
    R134 |
    R135 |
    R136 |
    R137 | 
    R140 |
    R141 |
    R142 |
    R143 |
    R144 |
    R145 |
    R146 |
    R147 | 
    R150 |
    R151 |
    R152 |
    R153 |
    R154 |
    R155 |
    R156 |
    R157 | 
    R160 |
    R161 |
    R162 |
    R163 |
    R164 |
    R165 |
    R166 |
    R167 | 
    R170 |
    R171 |
    R172 |
    R173 |
    R174 |
    R175 |
    R176 |
    R177 | 
    R200 |
    R201 |
    R202 |
    R203 |
    R204 |
    R205 |
    R206 |
    R207 | 
    R210 |
    R211 |
    R212 |
    R213 |
    R214 |
    R215 |
    R216 |
    R217 | 
    AL |
    CL |
    DL |
    BL |
    SIL |
    DIL |
    SPL |
    BPL |
    R8B |
    R9B |
    R10B |
    R11B |
    R12B |
    R13B |
    R14B |
    R15B |
    AH |
    CH |
    DH |
    BH |
    AX |
    CX |
    DX |
    BX |
    SI |
    DI |
    SP |
    BP |
    R8W |
    R9W |
    R10W |
    R11W |
    R12W |
    R13W |
    R14W |
    R15W |
    EAX |
    ECX |
    EDX |
    EBX |
    ESI |
    EDI |
    ESP |
    EBP |
    R8D |
    R9D |
    R10D |
    R11D |
    R12D |
    R13D |
    R14D |
    R15D |
    RAX |
    RCX |
    RDX |
    RBX |
    RSI |
    RDI |
    RSP |
    RBP |
    R8 |
    R9 |
    R10 |
    R11 |
    R12 |
    R13 |
    R14 |
    R15
    deriving (Eq, Ord)
