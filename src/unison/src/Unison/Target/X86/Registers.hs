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

registerArray = [RegisterClass GPR,
                 RegisterClass FPR,
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
registerAtoms FAX = (R004, R007) {- handy shorthand in function calls -}
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

-- | Register atoms of 8-byte floating-point registers (or 4-byte, or 16-byte, FIXME)

registerAtoms XMM0  = (R200, R207)
registerAtoms XMM1  = (R210, R217)
registerAtoms XMM2  = (R220, R227)
registerAtoms XMM3  = (R230, R237)
registerAtoms XMM4  = (R240, R247)
registerAtoms XMM5  = (R250, R257)
registerAtoms XMM6  = (R260, R267)
registerAtoms XMM7  = (R270, R277)
registerAtoms XMM8  = (R300, R307)
registerAtoms XMM9  = (R310, R317)
registerAtoms XMM10 = (R320, R327)
registerAtoms XMM11 = (R330, R337)
registerAtoms XMM12 = (R340, R347)
registerAtoms XMM13 = (R350, R357)
registerAtoms XMM14 = (R360, R367)
registerAtoms XMM15 = (R370, R377)

-- | Register atoms of 16-byte "registers"

registerAtoms RCX_RDX = (R010, R027)
registerAtoms RSI_RDI = (R040, R057)
registerAtoms R8_R9 = (R100, R117)
registerAtoms R10_R11 = (R120, R137)

-- not really in the register array
registerAtoms EFLAGS = (R400, R407)
registerAtoms RIP = (R410, R417)

registerAtoms r = error ("unmatched: registerAtoms " ++ show r)

-- | Register classes
regClasses =
    map RegisterClass [CCR, GR8, GR8_NOREX, GR16, GR32, GR32_NOREX, GR32_NOAX, GR32_AUX, GR64, GR64_NOSP, GR128_AUX,
                       Ptr_rc, Ptr_rc_nosp, Ptr_rc_norex, Ptr_rc_norex_nosp, Ptr_rc_tailcall,
                       FR32, FR64, VR128,
                       AUX] ++
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

registers (RegisterClass FPR) =
    [R200, R201, R202, R203, R204, R205, R206, R207,
     R210, R211, R212, R213, R214, R215, R216, R217,
     R220, R221, R222, R223, R224, R225, R226, R227,
     R230, R231, R232, R233, R234, R235, R236, R237,
     R240, R241, R242, R243, R244, R245, R246, R247,
     R250, R251, R252, R253, R254, R255, R256, R257,
     R260, R261, R262, R263, R264, R265, R266, R267,
     R270, R271, R272, R273, R274, R275, R276, R277,
     R300, R301, R302, R303, R304, R305, R306, R307,
     R310, R311, R312, R313, R314, R315, R316, R317,
     R320, R321, R322, R323, R324, R325, R326, R327,
     R330, R331, R332, R333, R334, R335, R336, R337,
     R340, R341, R342, R343, R344, R345, R346, R347,
     R350, R351, R352, R353, R354, R355, R356, R357,
     R360, R361, R362, R363, R364, R365, R366, R367,
     R370, R371, R372, R373, R374, R375, R376, R377]

registers (RegisterClass CCR) =
    [R400, R401, R402, R403, R404, R405, R406, R407,
     R410, R411, R412, R413, R414, R415, R416, R417]

registers (RegisterClass GR8) =
    [AL, AH, CL, CH, DL, DH, BL, BH, SIL, DIL, SPL, BPL, R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B]

registers (RegisterClass GR8_NOREX) =
    [AL, AH, CL, CH, DL, DH, BL, BH                                                                  ]

registers (RegisterClass GR16) =
    [AX, CX, DX, BX, SI, DI, SP, BP, R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W]

registers (RegisterClass GR32) =
    [EAX, ECX, EDX, EBX, ESI, EDI, ESP, EBP, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D]

registers (RegisterClass GR32_NOREX) =
    [EAX, ECX, EDX, EBX, ESI, EDI, ESP, EBP                                              ]

registers (RegisterClass GR32_NOAX) =
    [     ECX, EDX, EBX, ESI, EDI, ESP, EBP, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D]

registers (RegisterClass GR32_AUX) =
    [FAX]

registers (RegisterClass GR64) = {- llvm/lib/Target/X86/X86RegisterInfo includes RIP, saying it's inaccurate -}
    [RAX, RCX, RDX, RBX, RSI, RDI, RSP, RBP, R8, R9, R10, R11, R12, R13, R14, R15, RIP]

registers (RegisterClass GR64_NOSP) =
    [RAX, RCX, RDX, RBX, RSI, RDI,      RBP, R8, R9, R10, R11, R12, R13, R14, R15     ]

registers (RegisterClass GR128_AUX) =
    [RCX_RDX, RSI_RDI, R8_R9, R10_R11]

registers (RegisterClass Ptr_rc) =
    registers (RegisterClass GR64)

registers (RegisterClass Ptr_rc_nosp) =
    registers (RegisterClass GR64_NOSP)

registers (RegisterClass Ptr_rc_norex) =
    [RAX, RCX, RDX, RBX, RSI, RDI, RSP, RBP,                                       RIP]

registers (RegisterClass Ptr_rc_norex_nosp) =
    [RAX, RCX, RDX, RBX, RSI, RDI,      RBP                                      ]

registers (RegisterClass Ptr_rc_tailcall) =
    [RAX, RCX, RDX,      RSI, RDI,           R8, R9, R11, RIP                    ]

registers (RegisterClass FR32) =
    [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15]

registers (RegisterClass FR64) =
    registers (RegisterClass FR32)

registers (RegisterClass VR128) =
    registers (RegisterClass FR32)

registers (RegisterClass AUX) =
    [EFLAGS, RIP]

registers (RegisterClass ALL) =
    registers (RegisterClass GPR)

registers rc = error ("unmatched: registers " ++ show rc)

-- | Index type (low/high/copy) of subregisters

subRegIndexType sr
    | sr == (NamedSubRegIndex "sub_32bit") ||
      sr == (NamedSubRegIndex "sub_16bit") ||
      sr == (NamedSubRegIndex "sub_8bit") ||
      sr == (NamedSubRegIndex "sub_8bit_hi") ||
      sr == (RawSubRegIndex 4) = LowSubRegIndex
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

reserved = [RSP, RIP]

-- | Caller- and callee-saved registers

-- | Registers that are not preserved across calls
callerSaved = [RAX, RCX, RDX, RDI, RSI, R8, R9, R10, R11]

-- | Registers that are preserved across calls
calleeSaved = [RBX, RBP, R12, R13, R14, R15]

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
  [(EFLAGS, "eflags"),
   (RIP, "rip"),
   (R000, "r000"),
   (R001, "r001"),
   (R002, "r002"),
   (R003, "r003"),
   (R004, "r004"),
   (R005, "r005"),
   (R006, "r006"),
   (R007, "r007"),
   (R010, "r010"),
   (R011, "r011"),
   (R012, "r012"),
   (R013, "r013"),
   (R014, "r014"),
   (R015, "r015"),
   (R016, "r016"),
   (R017, "r017"),
   (R020, "r020"),
   (R021, "r021"),
   (R022, "r022"),
   (R023, "r023"),
   (R024, "r024"),
   (R025, "r025"),
   (R026, "r026"),
   (R027, "r027"),
   (R030, "r030"),
   (R031, "r031"),
   (R032, "r032"),
   (R033, "r033"),
   (R034, "r034"),
   (R035, "r035"),
   (R036, "r036"),
   (R037, "r037"),
   (R040, "r040"),
   (R041, "r041"),
   (R042, "r042"),
   (R043, "r043"),
   (R044, "r044"),
   (R045, "r045"),
   (R046, "r046"),
   (R047, "r047"),
   (R050, "r050"),
   (R051, "r051"),
   (R052, "r052"),
   (R053, "r053"),
   (R054, "r054"),
   (R055, "r055"),
   (R056, "r056"),
   (R057, "r057"),
   (R060, "r060"),
   (R061, "r061"),
   (R062, "r062"),
   (R063, "r063"),
   (R064, "r064"),
   (R065, "r065"),
   (R066, "r066"),
   (R067, "r067"),
   (R070, "r070"),
   (R071, "r071"),
   (R072, "r072"),
   (R073, "r073"),
   (R074, "r074"),
   (R075, "r075"),
   (R076, "r076"),
   (R077, "r077"),
   (R100, "r100"),
   (R101, "r101"),
   (R102, "r102"),
   (R103, "r103"),
   (R104, "r104"),
   (R105, "r105"),
   (R106, "r106"),
   (R107, "r107"),
   (R110, "r110"),
   (R111, "r111"),
   (R112, "r112"),
   (R113, "r113"),
   (R114, "r114"),
   (R115, "r115"),
   (R116, "r116"),
   (R117, "r117"),
   (R120, "r120"),
   (R121, "r121"),
   (R122, "r122"),
   (R123, "r123"),
   (R124, "r124"),
   (R125, "r125"),
   (R126, "r126"),
   (R127, "r127"),
   (R130, "r130"),
   (R131, "r131"),
   (R132, "r132"),
   (R133, "r133"),
   (R134, "r134"),
   (R135, "r135"),
   (R136, "r136"),
   (R137, "r137"),
   (R140, "r140"),
   (R141, "r141"),
   (R142, "r142"),
   (R143, "r143"),
   (R144, "r144"),
   (R145, "r145"),
   (R146, "r146"),
   (R147, "r147"),
   (R150, "r150"),
   (R151, "r151"),
   (R152, "r152"),
   (R153, "r153"),
   (R154, "r154"),
   (R155, "r155"),
   (R156, "r156"),
   (R157, "r157"),
   (R160, "r160"),
   (R161, "r161"),
   (R162, "r162"),
   (R163, "r163"),
   (R164, "r164"),
   (R165, "r165"),
   (R166, "r166"),
   (R167, "r167"),
   (R170, "r170"),
   (R171, "r171"),
   (R172, "r172"),
   (R173, "r173"),
   (R174, "r174"),
   (R175, "r175"),
   (R176, "r176"),
   (R177, "r177"),
   (R200, "r200"),
   (R201, "r201"),
   (R202, "r202"),
   (R203, "r203"),
   (R204, "r204"),
   (R205, "r205"),
   (R206, "r206"),
   (R207, "r207"),
   (R210, "r210"),
   (R211, "r211"),
   (R212, "r212"),
   (R213, "r213"),
   (R214, "r214"),
   (R215, "r215"),
   (R216, "r216"),
   (R217, "r217"),
   (R220, "r220"),
   (R221, "r221"),
   (R222, "r222"),
   (R223, "r223"),
   (R224, "r224"),
   (R225, "r225"),
   (R226, "r226"),
   (R227, "r227"),
   (R230, "r230"),
   (R231, "r231"),
   (R232, "r232"),
   (R233, "r233"),
   (R234, "r234"),
   (R235, "r235"),
   (R236, "r236"),
   (R237, "r237"),
   (R240, "r240"),
   (R241, "r241"),
   (R242, "r242"),
   (R243, "r243"),
   (R244, "r244"),
   (R245, "r245"),
   (R246, "r246"),
   (R247, "r247"),
   (R250, "r250"),
   (R251, "r251"),
   (R252, "r252"),
   (R253, "r253"),
   (R254, "r254"),
   (R255, "r255"),
   (R256, "r256"),
   (R257, "r257"),
   (R260, "r260"),
   (R261, "r261"),
   (R262, "r262"),
   (R263, "r263"),
   (R264, "r264"),
   (R265, "r265"),
   (R266, "r266"),
   (R267, "r267"),
   (R270, "r270"),
   (R271, "r271"),
   (R272, "r272"),
   (R273, "r273"),
   (R274, "r274"),
   (R275, "r275"),
   (R276, "r276"),
   (R277, "r277"),
   (R300, "r300"),
   (R301, "r301"),
   (R302, "r302"),
   (R303, "r303"),
   (R304, "r304"),
   (R305, "r305"),
   (R306, "r306"),
   (R307, "r307"),
   (R310, "r310"),
   (R311, "r311"),
   (R312, "r312"),
   (R313, "r313"),
   (R314, "r314"),
   (R315, "r315"),
   (R316, "r316"),
   (R317, "r317"),
   (R320, "r320"),
   (R321, "r321"),
   (R322, "r322"),
   (R323, "r323"),
   (R324, "r324"),
   (R325, "r325"),
   (R326, "r326"),
   (R327, "r327"),
   (R330, "r330"),
   (R331, "r331"),
   (R332, "r332"),
   (R333, "r333"),
   (R334, "r334"),
   (R335, "r335"),
   (R336, "r336"),
   (R337, "r337"),
   (R340, "r340"),
   (R341, "r341"),
   (R342, "r342"),
   (R343, "r343"),
   (R344, "r344"),
   (R345, "r345"),
   (R346, "r346"),
   (R347, "r347"),
   (R350, "r350"),
   (R351, "r351"),
   (R352, "r352"),
   (R353, "r353"),
   (R354, "r354"),
   (R355, "r355"),
   (R356, "r356"),
   (R357, "r357"),
   (R360, "r360"),
   (R361, "r361"),
   (R362, "r362"),
   (R363, "r363"),
   (R364, "r364"),
   (R365, "r365"),
   (R366, "r366"),
   (R367, "r367"),
   (R370, "r370"),
   (R371, "r371"),
   (R372, "r372"),
   (R373, "r373"),
   (R374, "r374"),
   (R375, "r375"),
   (R376, "r376"),
   (R377, "r377"),
   (R400, "r400"),
   (R401, "r401"),
   (R402, "r402"),
   (R403, "r403"),
   (R404, "r404"),
   (R405, "r405"),
   (R406, "r406"),
   (R407, "r407"),
   (R410, "r410"),
   (R411, "r411"),
   (R412, "r412"),
   (R413, "r413"),
   (R414, "r414"),
   (R415, "r415"),
   (R416, "r416"),
   (R417, "r417"),
   (AL, "al"),
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
   (FAX, "fax"),
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
   (XMM0, "xmm0"),
   (XMM1, "xmm1"),
   (XMM2, "xmm2"),
   (XMM3, "xmm3"),
   (XMM4, "xmm4"),
   (XMM5, "xmm5"),
   (XMM6, "xmm6"),
   (XMM7, "xmm7"),
   (XMM8, "xmm8"),
   (XMM9, "xmm9"),
   (XMM10, "xmm10"),
   (XMM11, "xmm11"),
   (XMM12, "xmm12"),
   (XMM13, "xmm13"),
   (XMM14, "xmm14"),
   (XMM15, "xmm15"),
   (RCX_RDX, "rcx_rdx"),
   (RSI_RDI, "rsi_rdi"),
   (R8_R9, "r8_r9"),
   (R10_R11, "r10_r11")]

