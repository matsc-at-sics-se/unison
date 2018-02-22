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
module Unison.Target.X86.Registers
    (registerArray, registerAtoms, regClasses, registers,
     subRegIndexType, infRegClassUsage, infRegClassBound,
     reserved, callerSaved, calleeSaved) where

import qualified Data.Map as M

import Unison
import Unison.Target.X86.X86RegisterDecl
import Unison.Target.X86.X86RegisterClassDecl

-- | Register array

registerArray = [RegisterClass GR8,
                 RegisterClass CCR,
                 InfiniteRegisterClass M8,
                 InfiniteRegisterClass RM8]

-- | Register atoms of 1-byte registers

registerAtoms AL = (R000, R000)
registerAtoms CL = (R010, R010)
registerAtoms DL = (R020, R020)
registerAtoms BL = (R030, R030)
registerAtoms SIL = (R040, R040)
registerAtoms DIL = (R050, R050)
registerAtoms SPL = (R060, R060)
registerAtoms BPL = (R070, R070)
registerAtoms R8B = (R100, R100)
registerAtoms R9B = (R110, R110)
registerAtoms R10B = (R120, R120)
registerAtoms R11B = (R130, R130)
registerAtoms R12B = (R140, R140)
registerAtoms R13B = (R150, R150)
registerAtoms R14B = (R160, R160)
registerAtoms R15B = (R170, R170)

registerAtoms AH = (R001, R001)
registerAtoms CH = (R011, R011)
registerAtoms DH = (R021, R021)
registerAtoms BH = (R031, R031)

-- | Register atoms of 2-byte registers

registerAtoms AX = (R000, R001)
registerAtoms CX = (R010, R011)
registerAtoms DX = (R020, R021)
registerAtoms BX = (R030, R031)
registerAtoms SI = (R040, R041)
registerAtoms DI = (R050, R051)
registerAtoms SP = (R060, R061)
registerAtoms BP = (R070, R071)
registerAtoms R8W = (R100, R101)
registerAtoms R9W = (R110, R111)
registerAtoms R10W = (R120, R121)
registerAtoms R11W = (R130, R131)
registerAtoms R12W = (R140, R141)
registerAtoms R13W = (R150, R151)
registerAtoms R14W = (R160, R161)
registerAtoms R15W = (R170, R171)

-- | Register atoms of 4-byte registers

registerAtoms EAX = (R000, R003)
registerAtoms ECX = (R010, R013)
registerAtoms EDX = (R020, R023)
registerAtoms EBX = (R030, R033)
registerAtoms ESI = (R040, R043)
registerAtoms EDI = (R050, R053)
registerAtoms ESP = (R060, R063)
registerAtoms EBP = (R070, R073)
registerAtoms R8D = (R100, R103)
registerAtoms R9D = (R110, R113)
registerAtoms R10D = (R120, R123)
registerAtoms R11D = (R130, R133)
registerAtoms R12D = (R140, R143)
registerAtoms R13D = (R150, R153)
registerAtoms R14D = (R160, R163)
registerAtoms R15D = (R170, R173)

-- | Register atoms of 8-byte registers

registerAtoms RAX = (R000, R007)
registerAtoms RCX = (R010, R017)
registerAtoms RDX = (R020, R027)
registerAtoms RBX = (R030, R037)
registerAtoms RSI = (R040, R047)
registerAtoms RDI = (R050, R057)
registerAtoms RSP = (R060, R067)
registerAtoms RBP = (R070, R077)
registerAtoms R8 = (R100, R107)
registerAtoms R9 = (R110, R117)
registerAtoms R10 = (R120, R127)
registerAtoms R11 = (R130, R137)
registerAtoms R12 = (R140, R147)
registerAtoms R13 = (R150, R157)
registerAtoms R14 = (R160, R167)
registerAtoms R15 = (R170, R177)

registerAtoms EFLAGS = (EFLAGS, EFLAGS)

registerAtoms r = error ("unmatched: registerAtoms " ++ show r)

-- | Register classes
regClasses =
    map RegisterClass [GR8, GR8_NOREX, GR16, GR32, GR32_NOREX, GR32_NOAX, GR64, GR64_NOSP, CCR] ++
    map InfiniteRegisterClass [M8, M16, M32, M64, RM8, RM16, RM32, RM64]

-- | Individual registers of each register class (octal, internal names)

registers (RegisterClass GPR) =
    [R000, R001, R002, R003, R004, R005, R006, R007,
     R010, R011, R012, R013, R014, R015, R016, R017,
     R020, R021, R022, R023, R024, R025, R026, R027,
     R030, R031, R032, R033, R034, R035, R036, R037,
     R040, R041, R042, R043, R044, R045, R046, R047,
     R050, R051, R052, R053, R054, R055, R056, R057,
     R060, R061, R062, R063, R064, R065, R066, R067,
     R070, R071, R072, R073, R074, R075, R076, R077,
     R100, R101, R102, R103, R104, R105, R106, R107,
     R110, R111, R112, R113, R114, R115, R116, R117,
     R120, R121, R122, R123, R124, R125, R126, R127,
     R130, R131, R132, R133, R134, R135, R136, R137,
     R140, R141, R142, R143, R144, R145, R146, R147,
     R150, R151, R152, R153, R154, R155, R156, R157,
     R160, R161, R162, R163, R164, R165, R166, R167,
     R170, R171, R172, R173, R174, R175, R176, R177]

registers (RegisterClass GR8) =
    [AL, AH, CL, CH, DL, DH, BL, BH, SIL, DIL, SPL, BPL, R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B]

registers (RegisterClass GR8_NOREX) =
    [AL, AH, CL, CH, DL, DH, BL, BH, SIL, DIL, SPL, BPL                                              ]

registers (RegisterClass GR16) =
    [AX, CX, DX, BX, SI, DI, SP, BP, R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W]

registers (RegisterClass GR32) =
    [EAX, ECX, EDX, EBX, ESI, EDI, ESP, EBP, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D]

registers (RegisterClass GR32_NOREX) =
    [EAX, ECX, EDX, EBX, ESI, EDI, ESP, EBP                                              ]

registers (RegisterClass GR32_NOAX) =
    [     ECX, EDX, EBX, ESI, EDI, ESP, EBP, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D]

registers (RegisterClass GR64) =
    [RAX, RCX, RDX, RBX, RSI, RDI, RSP, RBP, R8, R9, R10, R11, R12, R13, R14, R15]

registers (RegisterClass GR64_NOSP) =
    [RAX, RCX, RDX, RBX, RSI, RDI,      RBP, R8, R9, R10, R11, R12, R13, R14, R15]

registers (RegisterClass CCR) =
    [EFLAGS]

registers (RegisterClass ALL) =
  registers (RegisterClass GPR)

registers rc = error ("unmatched: registers " ++ show rc)

-- | Index type (low/high/copy) of subregisters

-- FIXME: I have seen sub_8bit applied to a 32-bit register
subRegIndexType sr
    | sr == (NamedSubRegIndex "sub_32bit") ||
      sr == (NamedSubRegIndex "sub_16bit") ||
      sr == (NamedSubRegIndex "sub_8bit") = LowSubRegIndex
subRegIndexType subreg = error ("unmatched: subRegIndexType " ++ show subreg)

-- | Map from infinite register class to register usage

infRegClassUsage (InfiniteRegisterClass rc)
  | rc == M8 = 1
  | rc == M16 = 2
  | rc == M32 = 4
  | rc == M64 = 8
  | rc == RM8 = 1
  | rc == RM16 = 2
  | rc == RM32 = 4
  | rc == RM64 = 8

-- | Map from infinite register class to (possibly) register atom upper bound

infRegClassBound (InfiniteRegisterClass _) = Nothing

-- | Registers whose value cannot be moved around

reserved = [RSP]

-- | Caller- and callee-saved registers

-- | Registers that are not preserved across calls
callerSaved = [RAX, RCX, RDX, RBP, R8, R9, R10, R11, R12, R13, R14, R15]

-- | Registers that are preserved across calls
calleeSaved = [RBX, RDI, RSI]

instance Read X86Register where
  readsPrec _ s = [(readReg s, "")]

readReg s = case M.lookup s (inverseMap regStrings) of
              (Just r) -> r
              Nothing -> error $ "unmatched: readReg " ++ s

instance Show X86Register where
  show r = case M.lookup r regStrings of
             (Just s) -> s
             Nothing -> error $ "unmatched: show X86Register"

regStrings = M.fromList $
  [(AL, "al"),
   (CL, "cl"),
   (DL, "dl"),
   (BL, "bl"),
   (SIL, "sil"),
   (DIL, "dil"),
   (SPL, "spl"),
   (BPL, "bpl"),
   (R8B, "r8b"),
   (R9B, "r9b"),
   (R10B, "r10b"),
   (R11B, "r11b"),
   (R12B, "r12b"),
   (R13B, "r13b"),
   (R14B, "r14b"),
   (R15B, "r15b"),
   (AH, "ah"),
   (CH, "ch"),
   (DH, "dh"),
   (BH, "bh"),
   (AX, "ax"),
   (CX, "cx"),
   (DX, "dx"),
   (BX, "bx"),
   (SI, "si"),
   (DI, "di"),
   (SP, "sp"),
   (BP, "bp"),
   (R8W, "r8w"),
   (R9W, "r9w"),
   (R10W, "r10w"),
   (R11W, "r11w"),
   (R12W, "r12w"),
   (R13W, "r13w"),
   (R14W, "r14w"),
   (R15W, "r15w"),
   (EAX, "eax"),
   (ECX, "ecx"),
   (EDX, "edx"),
   (EBX, "ebx"),
   (ESI, "esi"),
   (EDI, "edi"),
   (ESP, "esp"),
   (EBP, "ebp"),
   (R8D, "r8d"),
   (R9D, "r9d"),
   (R10D, "r10d"),
   (R11D, "r11d"),
   (R12D, "r12d"),
   (R13D, "r13d"),
   (R14D, "r14d"),
   (R15D, "r15d"),
   (RAX, "rax"),
   (RCX, "rcx"),
   (RDX, "rdx"),
   (RBX, "rbx"),
   (RSI, "rsi"),
   (RDI, "rdi"),
   (RSP, "rsp"),
   (RBP, "rbp"),
   (R8, "r8"),
   (R9, "r9"),
   (R10, "r10"),
   (R11, "r11"),
   (R12, "r12"),
   (R13, "r13"),
   (R14, "r14"),
   (R15, "r15"),
   (EFLAGS, "eflags")]

