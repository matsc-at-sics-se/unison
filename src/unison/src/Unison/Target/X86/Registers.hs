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
     reserved, callerSaved, calleeSaved,
     regClassTable, regClassStringToLogSize, regClassToLogSize) where

import qualified Data.Map as M
import Data.List

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

registerAtoms AL = (AL, AL)
registerAtoms CL = (CL, CL)
registerAtoms DL = (DL, DL)
registerAtoms BL = (BL, BL)
registerAtoms SIL = (SIL, SIL)
registerAtoms DIL = (DIL, DIL)
registerAtoms SPL = (SPL, SPL)
registerAtoms BPL = (BPL, BPL)
registerAtoms R8B = (R8B, R8B)
registerAtoms R9B = (R9B, R9B)
registerAtoms R10B = (R10B, R10B)
registerAtoms R11B = (R11B, R11B)
registerAtoms R12B = (R12B, R12B)
registerAtoms R13B = (R13B, R13B)
registerAtoms R14B = (R14B, R14B)
registerAtoms R15B = (R15B, R15B)

registerAtoms AH = (AH, AH)
registerAtoms CH = (CH, CH)
registerAtoms DH = (DH, DH)
registerAtoms BH = (BH, BH)

-- | Register atoms of 2-byte registers

registerAtoms AX = (AL, AH)
registerAtoms AX_HI = (R002, R003)
registerAtoms CX = (CL, CH)
registerAtoms CX_HI = (R012, R013)
registerAtoms DX = (DL, DH)
registerAtoms DX_HI = (R022, R023)
registerAtoms BX = (BL, BH)
registerAtoms BX_HI = (R032, R033)
registerAtoms SI = (SIL, R041)
registerAtoms SI_HI = (R042, R043)
registerAtoms DI = (DIL, R051)
registerAtoms DI_HI = (R052, R053)
registerAtoms SP = (SPL, R061)
registerAtoms SP_HI = (R062, R063)
registerAtoms BP = (BPL, R071)
registerAtoms BP_HI = (R072, R073)
registerAtoms R8W = (R8B, R101)
registerAtoms R8W_HI = (R102, R103)
registerAtoms R9W = (R9B, R111)
registerAtoms R9W_HI = (R112, R113)
registerAtoms R10W = (R10B, R121)
registerAtoms R10W_HI = (R122, R123)
registerAtoms R11W = (R11B, R131)
registerAtoms R11W_HI = (R132, R133)
registerAtoms R12W = (R12B, R141)
registerAtoms R12W_HI = (R142, R143)
registerAtoms R13W = (R13B, R151)
registerAtoms R13W_HI = (R152, R153)
registerAtoms R14W = (R14B, R161)
registerAtoms R14W_HI = (R162, R163)
registerAtoms R15W = (R15B, R171)
registerAtoms R15W_HI = (R172, R173)

-- | Register atoms of 4-byte registers

registerAtoms EAX = (AL, R003)
registerAtoms EAX_HI = (R004, R007) {- handy shorthand in function calls -}
registerAtoms ECX = (CL, R013)
registerAtoms ECX_HI = (R014, R017)
registerAtoms EDX = (DL, R023)
registerAtoms EDX_HI = (R024, R027)
registerAtoms EBX = (BL, R033)
registerAtoms EBX_HI = (R034, R037)
registerAtoms ESI = (SIL, R043)
registerAtoms ESI_HI = (R044, R047)
registerAtoms EDI = (DIL, R053)
registerAtoms EDI_HI = (R054, R057)
registerAtoms ESP = (SPL, R063)
registerAtoms ESP_HI = (R064, R067)
registerAtoms EBP = (BPL, R073)
registerAtoms EBP_HI = (R074, R077)
registerAtoms R8D = (R8B, R103)
registerAtoms R8D_HI = (R104, R107)
registerAtoms R9D = (R9B, R113)
registerAtoms R9D_HI = (R114, R117)
registerAtoms R10D = (R10B, R123)
registerAtoms R10D_HI = (R124, R127)
registerAtoms R11D = (R11B, R133)
registerAtoms R11D_HI = (R134, R137)
registerAtoms R12D = (R12B, R143)
registerAtoms R12D_HI = (R144, R147)
registerAtoms R13D = (R13B, R153)
registerAtoms R13D_HI = (R154, R157)
registerAtoms R14D = (R14B, R163)
registerAtoms R14D_HI = (R164, R167)
registerAtoms R15D = (R15B, R173)
registerAtoms R15D_HI = (R174, R177)

-- | Register atoms of 8-byte registers

registerAtoms RAX = (AL, R007)
registerAtoms RCX = (CL, R017)
registerAtoms RDX = (DL, R027)
registerAtoms RBX = (BL, R037)
registerAtoms RSI = (SIL, R047)
registerAtoms RDI = (DIL, R057)
registerAtoms RSP = (SPL, R067)
registerAtoms RBP = (BPL, R077)
registerAtoms R8 = (R8B, R107)
registerAtoms R9 = (R9B, R117)
registerAtoms R10 = (R10B, R127)
registerAtoms R11 = (R11B, R137)
registerAtoms R12 = (R12B, R147)
registerAtoms R13 = (R13B, R157)
registerAtoms R14 = (R14B, R167)
registerAtoms R15 = (R15B, R177)

-- | Register atoms of 32-bit floating-point registers (UMM*, syntax %xmm*)

registerAtoms UMM0  = (R200, R203)
registerAtoms UMM0_HI  = (R204, R207)
registerAtoms UMM1  = (R240, R243)
registerAtoms UMM2  = (R300, R303)
registerAtoms UMM3  = (R340, R343)
registerAtoms UMM4  = (R400, R403)
registerAtoms UMM5  = (R440, R443)
registerAtoms UMM6  = (R500, R503)
registerAtoms UMM7  = (R540, R543)
registerAtoms UMM8  = (R600, R603)
registerAtoms UMM9  = (R640, R643)
registerAtoms UMM10 = (R700, R703)
registerAtoms UMM11 = (R740, R743)
registerAtoms UMM12 = (R1000, R1003)
registerAtoms UMM13 = (R1040, R1043)
registerAtoms UMM14 = (R1100, R1103)
registerAtoms UMM15 = (R1140, R1143)

-- | Register atoms of 64-bit floating-point registers (VMM*, syntax %xmm*)

registerAtoms VMM0  = (R200, R207)
registerAtoms VMM0_HI  = (R210, R217)
registerAtoms VMM1  = (R240, R247)
registerAtoms VMM1_HI  = (R250, R257)
registerAtoms VMM2  = (R300, R307)
registerAtoms VMM2_HI  = (R310, R317)
registerAtoms VMM3  = (R340, R347)
registerAtoms VMM3_HI  = (R350, R357)
registerAtoms VMM4  = (R400, R407)
registerAtoms VMM4_HI  = (R410, R417)
registerAtoms VMM5  = (R440, R447)
registerAtoms VMM5_HI  = (R450, R457)
registerAtoms VMM6  = (R500, R507)
registerAtoms VMM6_HI  = (R510, R517)
registerAtoms VMM7  = (R540, R547)
registerAtoms VMM7_HI  = (R550, R557)
registerAtoms VMM8  = (R600, R607)
registerAtoms VMM8_HI  = (R610, R617)
registerAtoms VMM9  = (R640, R647)
registerAtoms VMM9_HI  = (R650, R657)
registerAtoms VMM10 = (R700, R707)
registerAtoms VMM10_HI = (R710, R717)
registerAtoms VMM11 = (R740, R747)
registerAtoms VMM11_HI = (R750, R757)
registerAtoms VMM12 = (R1000, R1007)
registerAtoms VMM12_HI = (R1010, R1017)
registerAtoms VMM13 = (R1040, R1047)
registerAtoms VMM13_HI = (R1050, R1057)
registerAtoms VMM14 = (R1100, R1107)
registerAtoms VMM14_HI = (R1110, R1117)
registerAtoms VMM15 = (R1140, R1147)
registerAtoms VMM15_HI = (R1150, R1157)

-- | Register atoms of 128-bit floating-point registers (WMM*, syntax %xmm*)

registerAtoms WMM0  = (R200, R217)
registerAtoms WMM0_HI  = (R220, R237)
registerAtoms WMM1  = (R240, R257)
registerAtoms WMM1_HI  = (R260, R277)
registerAtoms WMM2  = (R300, R317)
registerAtoms WMM2_HI  = (R320, R337)
registerAtoms WMM3  = (R340, R357)
registerAtoms WMM3_HI  = (R360, R377)
registerAtoms WMM4  = (R400, R417)
registerAtoms WMM4_HI  = (R420, R437)
registerAtoms WMM5  = (R440, R457)
registerAtoms WMM5_HI  = (R460, R477)
registerAtoms WMM6  = (R500, R517)
registerAtoms WMM6_HI  = (R520, R537)
registerAtoms WMM7  = (R540, R557)
registerAtoms WMM7_HI  = (R560, R577)
registerAtoms WMM8  = (R600, R617)
registerAtoms WMM8_HI  = (R620, R637)
registerAtoms WMM9  = (R640, R657)
registerAtoms WMM9_HI  = (R660, R677)
registerAtoms WMM10 = (R700, R717)
registerAtoms WMM10_HI  = (R720, R737)
registerAtoms WMM11 = (R740, R757)
registerAtoms WMM11_HI  = (R760, R777)
registerAtoms WMM12 = (R1000, R1017)
registerAtoms WMM12_HI  = (R1020, R1037)
registerAtoms WMM13 = (R1040, R1057)
registerAtoms WMM13_HI  = (R1060, R1077)
registerAtoms WMM14 = (R1100, R1117)
registerAtoms WMM14_HI  = (R1120, R1137)
registerAtoms WMM15 = (R1140, R1157)
registerAtoms WMM15_HI  = (R1160, R1177)

-- | Register atoms of ambiguous floating-point registers (XXM*, syntax %xmm*)

registerAtoms XMM0  = (R200, R217)
registerAtoms XMM1  = (R240, R257)
registerAtoms XMM2  = (R300, R317)
registerAtoms XMM3  = (R340, R357)
registerAtoms XMM4  = (R400, R417)
registerAtoms XMM5  = (R440, R457)
registerAtoms XMM6  = (R500, R517)
registerAtoms XMM7  = (R540, R557)
registerAtoms XMM8  = (R600, R617)
registerAtoms XMM9  = (R640, R657)
registerAtoms XMM10 = (R700, R717)
registerAtoms XMM11 = (R740, R757)
registerAtoms XMM12 = (R1000, R1017)
registerAtoms XMM13 = (R1040, R1057)
registerAtoms XMM14 = (R1100, R1117)
registerAtoms XMM15 = (R1140, R1157)

-- | Register atoms of 256-bit floating-point registers (YMM)

registerAtoms YMM0  = (R200, R237)
registerAtoms YMM1  = (R240, R277)
registerAtoms YMM2  = (R300, R337)
registerAtoms YMM3  = (R340, R377)
registerAtoms YMM4  = (R400, R437)
registerAtoms YMM5  = (R440, R477)
registerAtoms YMM6  = (R500, R537)
registerAtoms YMM7  = (R540, R577)
registerAtoms YMM8  = (R600, R637)
registerAtoms YMM9  = (R640, R677)
registerAtoms YMM10 = (R700, R737)
registerAtoms YMM11 = (R740, R777)
registerAtoms YMM12 = (R1000, R1037)
registerAtoms YMM13 = (R1040, R1077)
registerAtoms YMM14 = (R1100, R1137)
registerAtoms YMM15 = (R1140, R1177)

-- | Register atoms of 16-byte caller-saved "registers"

registerAtoms RCX_RDX = (CL, R027)
registerAtoms RSI_RDI = (SIL, R057)
registerAtoms R8_R9 = (R8B, R117)
registerAtoms R10_R11 = (R10B, R137)

-- | Giant caller-saved XMM register atom

registerAtoms YMM1_15 = (R240, R1177)

-- not really in the register array
registerAtoms EFLAGS = (R1200, R1207)
registerAtoms RIP = (R1210, R1217)

registerAtoms r = error ("unmatched: registerAtoms " ++ show r)

-- | Register classes
regClasses =
    map RegisterClass [CCR, GR8, GR8_NOREX, GR16, GR16_AUX, GR32, GR32_NOREX, GR32_NOAX, GR32_AUX, GR64, GR64_NOSP, GR32orGR64,
                       GR128_AUX,
                       Ptr_rc, Ptr_rc_nosp, Ptr_rc_norex, Ptr_rc_norex_nosp, Ptr_rc_tailcall,
                       FR32, FR64, FR128, VR128, VR256, FR32_AUX, FR64_AUX, FR128_AUX, VR2048_AUX, AMBIG,
                       AUXE, AUXR, AUXB] ++
    map InfiniteRegisterClass [M8, M16, M32, M64, M128, M256, RM8, RM16, RM32, RM64, RM128, RM256]

-- | Individual registers of each register class (octal, internal names)

registers (RegisterClass GPR) =
    [AL, AH, R002, R003, R004, R005, R006, R007,
     CL, CH, R012, R013, R014, R015, R016, R017,
     DL, DH, R022, R023, R024, R025, R026, R027,
     BL, BH, R032, R033, R034, R035, R036, R037,
     SIL, R041, R042, R043, R044, R045, R046, R047,
     DIL, R051, R052, R053, R054, R055, R056, R057,
     SPL, R061, R062, R063, R064, R065, R066, R067,
     BPL, R071, R072, R073, R074, R075, R076, R077,
     R8B, R101, R102, R103, R104, R105, R106, R107,
     R9B, R111, R112, R113, R114, R115, R116, R117,
     R10B, R121, R122, R123, R124, R125, R126, R127,
     R11B, R131, R132, R133, R134, R135, R136, R137,
     R12B, R141, R142, R143, R144, R145, R146, R147,
     R13B, R151, R152, R153, R154, R155, R156, R157,
     R14B, R161, R162, R163, R164, R165, R166, R167,
     R15B, R171, R172, R173, R174, R175, R176, R177]

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
     R370, R371, R372, R373, R374, R375, R376, R377,
     R400, R401, R402, R403, R404, R405, R406, R407,
     R410, R411, R412, R413, R414, R415, R416, R417,
     R420, R421, R422, R423, R424, R425, R426, R427,
     R430, R431, R432, R433, R434, R435, R436, R437,
     R440, R441, R442, R443, R444, R445, R446, R447,
     R450, R451, R452, R453, R454, R455, R456, R457,
     R460, R461, R462, R463, R464, R465, R466, R467,
     R470, R471, R472, R473, R474, R475, R476, R477,
     R500, R501, R502, R503, R504, R505, R506, R507,
     R510, R511, R512, R513, R514, R515, R516, R517,
     R520, R521, R522, R523, R524, R525, R526, R527,
     R530, R531, R532, R533, R534, R535, R536, R537,
     R540, R541, R542, R543, R544, R545, R546, R547,
     R550, R551, R552, R553, R554, R555, R556, R557,
     R560, R561, R562, R563, R564, R565, R566, R567,
     R570, R571, R572, R573, R574, R575, R576, R577,
     R600, R601, R602, R603, R604, R605, R606, R607,
     R610, R611, R612, R613, R614, R615, R616, R617,
     R620, R621, R622, R623, R624, R625, R626, R627,
     R630, R631, R632, R633, R634, R635, R636, R637,
     R640, R641, R642, R643, R644, R645, R646, R647,
     R650, R651, R652, R653, R654, R655, R656, R657,
     R660, R661, R662, R663, R664, R665, R666, R667,
     R670, R671, R672, R673, R674, R675, R676, R677,
     R700, R701, R702, R703, R704, R705, R706, R707,
     R710, R711, R712, R713, R714, R715, R716, R717,
     R720, R721, R722, R723, R724, R725, R726, R727,
     R730, R731, R732, R733, R734, R735, R736, R737,
     R740, R741, R742, R743, R744, R745, R746, R747,
     R750, R751, R752, R753, R754, R755, R756, R757,
     R760, R761, R762, R763, R764, R765, R766, R767,
     R770, R771, R772, R773, R774, R775, R776, R777,
     R1000, R1001, R1002, R1003, R1004, R1005, R1006, R1007,
     R1010, R1011, R1012, R1013, R1014, R1015, R1016, R1017,
     R1020, R1021, R1022, R1023, R1024, R1025, R1026, R1027,
     R1030, R1031, R1032, R1033, R1034, R1035, R1036, R1037,
     R1040, R1041, R1042, R1043, R1044, R1045, R1046, R1047,
     R1050, R1051, R1052, R1053, R1054, R1055, R1056, R1057,
     R1060, R1061, R1062, R1063, R1064, R1065, R1066, R1067,
     R1070, R1071, R1072, R1073, R1074, R1075, R1076, R1077,
     R1100, R1101, R1102, R1103, R1104, R1105, R1106, R1107,
     R1110, R1111, R1112, R1113, R1114, R1115, R1116, R1117,
     R1120, R1121, R1122, R1123, R1124, R1125, R1126, R1127,
     R1130, R1131, R1132, R1133, R1134, R1135, R1136, R1137,
     R1140, R1141, R1142, R1143, R1144, R1145, R1146, R1147,
     R1150, R1151, R1152, R1153, R1154, R1155, R1156, R1157,
     R1160, R1161, R1162, R1163, R1164, R1165, R1166, R1167,
     R1170, R1171, R1172, R1173, R1174, R1175, R1176, R1177]

registers (RegisterClass CCR) =
    [R1200, R1201, R1202, R1203, R1204, R1205, R1206, R1207,
     R1210, R1211, R1212, R1213, R1214, R1215, R1216, R1217]

registers (RegisterClass GR8) =
    [AL, AH, CL, CH, DL, DH, BL, BH, SIL, DIL, SPL, BPL, R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B]

registers (RegisterClass GR8_NOREX) =
    [AL, AH, CL, CH, DL, DH, BL, BH                                                                  ]

registers (RegisterClass GR16) =
    [AX, CX, DX, BX, SI, DI, SP, BP, R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W]

registers (RegisterClass GR16_AUX) =
    [AX_HI, CX_HI, DX_HI, BX_HI, SI_HI, DI_HI, SP_HI, BP_HI,
     R8W_HI, R9W_HI, R10W_HI, R11W_HI, R12W_HI, R13W_HI, R14W_HI, R15W_HI]

registers (RegisterClass GR32) =
    [EAX, ECX, EDX, EBX, ESI, EDI, ESP, EBP, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D]

registers (RegisterClass GR32_NOREX) =
    [EAX, ECX, EDX, EBX, ESI, EDI, ESP, EBP                                              ]

registers (RegisterClass GR32_NOAX) =
    [     ECX, EDX, EBX, ESI, EDI, ESP, EBP, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D]

registers (RegisterClass GR32_AUX) =
    [EAX_HI, ECX_HI, EDX_HI, EBX_HI, ESI_HI, EDI_HI, ESP_HI, EBP_HI,
     R8D_HI, R9D_HI, R10D_HI, R11D_HI, R12D_HI, R13D_HI, R14D_HI, R15D_HI]

registers (RegisterClass GR64) = {- llvm/lib/Target/X86/X86RegisterInfo includes RIP, saying it's inaccurate -}
    [RAX, RCX, RDX, RBX, RSI, RDI, RSP, RBP, R8, R9, R10, R11, R12, R13, R14, R15, RIP]

registers (RegisterClass GR64_NOSP) =
    [RAX, RCX, RDX, RBX, RSI, RDI,      RBP, R8, R9, R10, R11, R12, R13, R14, R15     ]

registers (RegisterClass GR32orGR64) =
    registers (RegisterClass GR64)

registers (RegisterClass GR128_AUX) =
    [RCX_RDX, RSI_RDI, R8_R9, R10_R11]

registers (RegisterClass VR2048_AUX) =
    [YMM1_15]

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
    [UMM0, UMM1, UMM2, UMM3, UMM4, UMM5, UMM6, UMM7,
     UMM8, UMM9, UMM10, UMM11, UMM12, UMM13, UMM14, UMM15]

registers (RegisterClass FR32_AUX) =
    [UMM0_HI]

registers (RegisterClass FR64) =
    [VMM0, VMM1, VMM2, VMM3, VMM4, VMM5, VMM6, VMM7,
     VMM8, VMM9, VMM10, VMM11, VMM12, VMM13, VMM14, VMM15]

registers (RegisterClass FR64_AUX) =
    [VMM0_HI, VMM1_HI, VMM2_HI, VMM3_HI, VMM4_HI, VMM5_HI, VMM6_HI, VMM7_HI,
     VMM8_HI, VMM9_HI, VMM10_HI, VMM11_HI, VMM12_HI, VMM13_HI, VMM14_HI, VMM15_HI]

registers (RegisterClass FR128) =
    [WMM0, WMM1, WMM2, WMM3, WMM4, WMM5, WMM6, WMM7,
     WMM8, WMM9, WMM10, WMM11, WMM12, WMM13, WMM14, WMM15]

registers (RegisterClass FR128_AUX) =
    [WMM0_HI, WMM1_HI, WMM2_HI, WMM3_HI, WMM4_HI, WMM5_HI, WMM6_HI, WMM7_HI,
     WMM8_HI, WMM9_HI, WMM10_HI, WMM11_HI, WMM12_HI, WMM13_HI, WMM14_HI, WMM15_HI]

registers (RegisterClass VR128) =
    registers (RegisterClass FR128)

registers (RegisterClass VR256) =
    [YMM0, YMM1, YMM2, YMM3, YMM4, YMM5, YMM6, YMM7, YMM8, YMM9, YMM10, YMM11, YMM12, YMM13, YMM14, YMM15]

registers (RegisterClass AMBIG) =
    [XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
     XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15]

registers (RegisterClass AUXE) =
    [EFLAGS]

registers (RegisterClass AUXR) =
    [RIP]

registers (RegisterClass AUXB) =
    [RBX]

registers (RegisterClass ALL) =
    registers (RegisterClass GPR) ++ registers (RegisterClass VR128)

registers rc = error ("unmatched: registers " ++ show rc)

-- | Index type (low/high/copy) of subregisters. The raw subregister indices are
-- taken from X86GenRegisterInfo.inc in LLVM's build directory.

subRegIndexType rc sr
    | sr == (NamedSubRegIndex "sub_xmm") || sr == (RawSubRegIndex 5) =
        case rc of
         "vr256"     -> [LowSubRegIndex]
         _           -> error ("SubRegIndexType: missing register class " ++ show rc ++ " " ++ show sr) 

subRegIndexType rc sr
    | sr == (NamedSubRegIndex "sub_32bit") || sr == (RawSubRegIndex 4) =
        case rc of
         "gr64"      -> [LowSubRegIndex]
         "gr64_nosp" -> [LowSubRegIndex]
         "gr64_norex_nosp" -> [LowSubRegIndex]
         "gr64_with_sub_8bit" -> [LowSubRegIndex]
         _           -> error ("SubRegIndexType: missing register class " ++ show rc ++ " " ++ show sr) 

subRegIndexType rc sr
    | sr == (NamedSubRegIndex "sub_16bit") || sr == (RawSubRegIndex 3) =
        case rc of
         "gr64"      -> [LowSubRegIndex, LowSubRegIndex]
         "gr64_nosp" -> [LowSubRegIndex, LowSubRegIndex]
         "gr64_norex_nosp" -> [LowSubRegIndex, LowSubRegIndex]
         "gr32"      -> [LowSubRegIndex]
         "gr32_nosp" -> [LowSubRegIndex]
         _           -> error ("SubRegIndexType: missing register class " ++ show rc ++ " " ++ show sr) 

subRegIndexType rc sr
    | sr == (NamedSubRegIndex "sub_8bit") || sr == (RawSubRegIndex 1) =
        case rc of
         "gr64"      -> [LowSubRegIndex, LowSubRegIndex, LowSubRegIndex]
         "gr64_nosp" -> [LowSubRegIndex, LowSubRegIndex, LowSubRegIndex]
         "gr64_norex_nosp" -> [LowSubRegIndex, LowSubRegIndex, LowSubRegIndex]
         "gr64_with_sub_8bit" -> [LowSubRegIndex, LowSubRegIndex, LowSubRegIndex]
         "gr32"      -> [LowSubRegIndex, LowSubRegIndex]
         "gr32_nosp" -> [LowSubRegIndex, LowSubRegIndex]
         "gr16"      -> [LowSubRegIndex]
         "gr16_nosp" -> [LowSubRegIndex]
         _           -> error ("SubRegIndexType: missing register class " ++ show rc ++ " " ++ show sr) 

subRegIndexType rc sr
    | sr == (NamedSubRegIndex "sub_8bit_hi") || sr == (RawSubRegIndex 2) =
        case rc of
         "gr64"      -> [HighSubRegIndex, LowSubRegIndex, LowSubRegIndex]
         "gr64_nosp" -> [HighSubRegIndex, LowSubRegIndex, LowSubRegIndex]
         "gr64_norex_nosp" -> [HighSubRegIndex, LowSubRegIndex, LowSubRegIndex]
         "gr64_abcd" -> [HighSubRegIndex, LowSubRegIndex, LowSubRegIndex]
         "gr32"      -> [HighSubRegIndex, LowSubRegIndex]
         "gr32_nosp" -> [HighSubRegIndex, LowSubRegIndex]
         "gr32_abcd" -> [HighSubRegIndex, LowSubRegIndex]
         "gr16"      -> [HighSubRegIndex]
         "gr16_nosp" -> [HighSubRegIndex]
         _           -> error ("SubRegIndexType: missing register class " ++ show rc ++ " " ++ show sr) 

subRegIndexType rc sr = error ("unmatched: subRegIndexType " ++ show (rc, sr))

-- | Map from infinite register class to register usage

infRegClassUsage (InfiniteRegisterClass rc)
  | rc == M8 = 1
  | rc == M16 = 2
  | rc == M32 = 4
  | rc == M64 = 8
  | rc == M128 = 16
  | rc == M256 = 32
  | rc == RM8 = 1
  | rc == RM16 = 2
  | rc == RM32 = 4
  | rc == RM64 = 8
  | rc == RM128 = 16
  | rc == RM256 = 32

-- | Map from infinite register class to (possibly) register atom upper bound

infRegClassBound (InfiniteRegisterClass _) = Nothing

-- | Registers whose value cannot be moved around

reserved = [RSP, RIP]

-- | Caller- and callee-saved registers

-- | Registers that are not preserved across calls
-- callerSaved = [RAX, RCX, RDX, RDI, RSI, R8, R9, R10, R11] ++ registers (RegisterClass VR128)
callerSaved = [RAX, RCX, RDX, RDI, RSI, R8, R9, R10, R11, YMM0, YMM1_15]

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
   (AL, "al"),
   (AH, "ah"),
   (R002, "r002"),
   (R003, "r003"),
   (R004, "r004"),
   (R005, "r005"),
   (R006, "r006"),
   (R007, "r007"),
   (CL, "cl"),
   (CH, "ch"),
   (R012, "r012"),
   (R013, "r013"),
   (R014, "r014"),
   (R015, "r015"),
   (R016, "r016"),
   (R017, "r017"),
   (DL, "dl"),
   (DH, "dh"),
   (R022, "r022"),
   (R023, "r023"),
   (R024, "r024"),
   (R025, "r025"),
   (R026, "r026"),
   (R027, "r027"),
   (BL, "bl"),
   (BH, "bh"),
   (R032, "r032"),
   (R033, "r033"),
   (R034, "r034"),
   (R035, "r035"),
   (R036, "r036"),
   (R037, "r037"),
   (SIL, "sil"),
   (R041, "r041"),
   (R042, "r042"),
   (R043, "r043"),
   (R044, "r044"),
   (R045, "r045"),
   (R046, "r046"),
   (R047, "r047"),
   (DIL, "dil"),
   (R051, "r051"),
   (R052, "r052"),
   (R053, "r053"),
   (R054, "r054"),
   (R055, "r055"),
   (R056, "r056"),
   (R057, "r057"),
   (SPL, "spl"),
   (R061, "r061"),
   (R062, "r062"),
   (R063, "r063"),
   (R064, "r064"),
   (R065, "r065"),
   (R066, "r066"),
   (R067, "r067"),
   (BPL, "bpl"),
   (R071, "r071"),
   (R072, "r072"),
   (R073, "r073"),
   (R074, "r074"),
   (R075, "r075"),
   (R076, "r076"),
   (R077, "r077"),
   (R8B, "r8b"),
   (R101, "r101"),
   (R102, "r102"),
   (R103, "r103"),
   (R104, "r104"),
   (R105, "r105"),
   (R106, "r106"),
   (R107, "r107"),
   (R9B, "r9b"),
   (R111, "r111"),
   (R112, "r112"),
   (R113, "r113"),
   (R114, "r114"),
   (R115, "r115"),
   (R116, "r116"),
   (R117, "r117"),
   (R10B, "r10b"),
   (R121, "r121"),
   (R122, "r122"),
   (R123, "r123"),
   (R124, "r124"),
   (R125, "r125"),
   (R126, "r126"),
   (R127, "r127"),
   (R11B, "r11b"),
   (R131, "r131"),
   (R132, "r132"),
   (R133, "r133"),
   (R134, "r134"),
   (R135, "r135"),
   (R136, "r136"),
   (R137, "r137"),
   (R12B, "r12b"),
   (R141, "r141"),
   (R142, "r142"),
   (R143, "r143"),
   (R144, "r144"),
   (R145, "r145"),
   (R146, "r146"),
   (R147, "r147"),
   (R13B, "r13b"),
   (R151, "r151"),
   (R152, "r152"),
   (R153, "r153"),
   (R154, "r154"),
   (R155, "r155"),
   (R156, "r156"),
   (R157, "r157"),
   (R14B, "r14b"),
   (R161, "r161"),
   (R162, "r162"),
   (R163, "r163"),
   (R164, "r164"),
   (R165, "r165"),
   (R166, "r166"),
   (R167, "r167"),
   (R15B, "r15b"),
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
   (R420, "r420"),
   (R421, "r421"),
   (R422, "r422"),
   (R423, "r423"),
   (R424, "r424"),
   (R425, "r425"),
   (R426, "r426"),
   (R427, "r427"),
   (R430, "r430"),
   (R431, "r431"),
   (R432, "r432"),
   (R433, "r433"),
   (R434, "r434"),
   (R435, "r435"),
   (R436, "r436"),
   (R437, "r437"),
   (R440, "r440"),
   (R441, "r441"),
   (R442, "r442"),
   (R443, "r443"),
   (R444, "r444"),
   (R445, "r445"),
   (R446, "r446"),
   (R447, "r447"),
   (R450, "r450"),
   (R451, "r451"),
   (R452, "r452"),
   (R453, "r453"),
   (R454, "r454"),
   (R455, "r455"),
   (R456, "r456"),
   (R457, "r457"),
   (R460, "r460"),
   (R461, "r461"),
   (R462, "r462"),
   (R463, "r463"),
   (R464, "r464"),
   (R465, "r465"),
   (R466, "r466"),
   (R467, "r467"),
   (R470, "r470"),
   (R471, "r471"),
   (R472, "r472"),
   (R473, "r473"),
   (R474, "r474"),
   (R475, "r475"),
   (R476, "r476"),
   (R477, "r477"),
   (R500, "r500"),
   (R501, "r501"),
   (R502, "r502"),
   (R503, "r503"),
   (R504, "r504"),
   (R505, "r505"),
   (R506, "r506"),
   (R507, "r507"),
   (R510, "r510"),
   (R511, "r511"),
   (R512, "r512"),
   (R513, "r513"),
   (R514, "r514"),
   (R515, "r515"),
   (R516, "r516"),
   (R517, "r517"),
   (R520, "r520"),
   (R521, "r521"),
   (R522, "r522"),
   (R523, "r523"),
   (R524, "r524"),
   (R525, "r525"),
   (R526, "r526"),
   (R527, "r527"),
   (R530, "r530"),
   (R531, "r531"),
   (R532, "r532"),
   (R533, "r533"),
   (R534, "r534"),
   (R535, "r535"),
   (R536, "r536"),
   (R537, "r537"),
   (R540, "r540"),
   (R541, "r541"),
   (R542, "r542"),
   (R543, "r543"),
   (R544, "r544"),
   (R545, "r545"),
   (R546, "r546"),
   (R547, "r547"),
   (R550, "r550"),
   (R551, "r551"),
   (R552, "r552"),
   (R553, "r553"),
   (R554, "r554"),
   (R555, "r555"),
   (R556, "r556"),
   (R557, "r557"),
   (R560, "r560"),
   (R561, "r561"),
   (R562, "r562"),
   (R563, "r563"),
   (R564, "r564"),
   (R565, "r565"),
   (R566, "r566"),
   (R567, "r567"),
   (R570, "r570"),
   (R571, "r571"),
   (R572, "r572"),
   (R573, "r573"),
   (R574, "r574"),
   (R575, "r575"),
   (R576, "r576"),
   (R577, "r577"),
   (R600, "r600"),
   (R601, "r601"),
   (R602, "r602"),
   (R603, "r603"),
   (R604, "r604"),
   (R605, "r605"),
   (R606, "r606"),
   (R607, "r607"),
   (R610, "r610"),
   (R611, "r611"),
   (R612, "r612"),
   (R613, "r613"),
   (R614, "r614"),
   (R615, "r615"),
   (R616, "r616"),
   (R617, "r617"),
   (R620, "r620"),
   (R621, "r621"),
   (R622, "r622"),
   (R623, "r623"),
   (R624, "r624"),
   (R625, "r625"),
   (R626, "r626"),
   (R627, "r627"),
   (R630, "r630"),
   (R631, "r631"),
   (R632, "r632"),
   (R633, "r633"),
   (R634, "r634"),
   (R635, "r635"),
   (R636, "r636"),
   (R637, "r637"),
   (R640, "r640"),
   (R641, "r641"),
   (R642, "r642"),
   (R643, "r643"),
   (R644, "r644"),
   (R645, "r645"),
   (R646, "r646"),
   (R647, "r647"),
   (R650, "r650"),
   (R651, "r651"),
   (R652, "r652"),
   (R653, "r653"),
   (R654, "r654"),
   (R655, "r655"),
   (R656, "r656"),
   (R657, "r657"),
   (R660, "r660"),
   (R661, "r661"),
   (R662, "r662"),
   (R663, "r663"),
   (R664, "r664"),
   (R665, "r665"),
   (R666, "r666"),
   (R667, "r667"),
   (R670, "r670"),
   (R671, "r671"),
   (R672, "r672"),
   (R673, "r673"),
   (R674, "r674"),
   (R675, "r675"),
   (R676, "r676"),
   (R677, "r677"),
   (R700, "r700"),
   (R701, "r701"),
   (R702, "r702"),
   (R703, "r703"),
   (R704, "r704"),
   (R705, "r705"),
   (R706, "r706"),
   (R707, "r707"),
   (R710, "r710"),
   (R711, "r711"),
   (R712, "r712"),
   (R713, "r713"),
   (R714, "r714"),
   (R715, "r715"),
   (R716, "r716"),
   (R717, "r717"),
   (R720, "r720"),
   (R721, "r721"),
   (R722, "r722"),
   (R723, "r723"),
   (R724, "r724"),
   (R725, "r725"),
   (R726, "r726"),
   (R727, "r727"),
   (R730, "r730"),
   (R731, "r731"),
   (R732, "r732"),
   (R733, "r733"),
   (R734, "r734"),
   (R735, "r735"),
   (R736, "r736"),
   (R737, "r737"),
   (R740, "r740"),
   (R741, "r741"),
   (R742, "r742"),
   (R743, "r743"),
   (R744, "r744"),
   (R745, "r745"),
   (R746, "r746"),
   (R747, "r747"),
   (R750, "r750"),
   (R751, "r751"),
   (R752, "r752"),
   (R753, "r753"),
   (R754, "r754"),
   (R755, "r755"),
   (R756, "r756"),
   (R757, "r757"),
   (R760, "r760"),
   (R761, "r761"),
   (R762, "r762"),
   (R763, "r763"),
   (R764, "r764"),
   (R765, "r765"),
   (R766, "r766"),
   (R767, "r767"),
   (R770, "r770"),
   (R771, "r771"),
   (R772, "r772"),
   (R773, "r773"),
   (R774, "r774"),
   (R775, "r775"),
   (R776, "r776"),
   (R777, "r777"),
   (R1000, "r1000"),
   (R1001, "r1001"),
   (R1002, "r1002"),
   (R1003, "r1003"),
   (R1004, "r1004"),
   (R1005, "r1005"),
   (R1006, "r1006"),
   (R1007, "r1007"),
   (R1010, "r1010"),
   (R1011, "r1011"),
   (R1012, "r1012"),
   (R1013, "r1013"),
   (R1014, "r1014"),
   (R1015, "r1015"),
   (R1016, "r1016"),
   (R1017, "r1017"),
   (R1020, "r1020"),
   (R1021, "r1021"),
   (R1022, "r1022"),
   (R1023, "r1023"),
   (R1024, "r1024"),
   (R1025, "r1025"),
   (R1026, "r1026"),
   (R1027, "r1027"),
   (R1030, "r1030"),
   (R1031, "r1031"),
   (R1032, "r1032"),
   (R1033, "r1033"),
   (R1034, "r1034"),
   (R1035, "r1035"),
   (R1036, "r1036"),
   (R1037, "r1037"),
   (R1040, "r1040"),
   (R1041, "r1041"),
   (R1042, "r1042"),
   (R1043, "r1043"),
   (R1044, "r1044"),
   (R1045, "r1045"),
   (R1046, "r1046"),
   (R1047, "r1047"),
   (R1050, "r1050"),
   (R1051, "r1051"),
   (R1052, "r1052"),
   (R1053, "r1053"),
   (R1054, "r1054"),
   (R1055, "r1055"),
   (R1056, "r1056"),
   (R1057, "r1057"),
   (R1060, "r1060"),
   (R1061, "r1061"),
   (R1062, "r1062"),
   (R1063, "r1063"),
   (R1064, "r1064"),
   (R1065, "r1065"),
   (R1066, "r1066"),
   (R1067, "r1067"),
   (R1070, "r1070"),
   (R1071, "r1071"),
   (R1072, "r1072"),
   (R1073, "r1073"),
   (R1074, "r1074"),
   (R1075, "r1075"),
   (R1076, "r1076"),
   (R1077, "r1077"),
   (R1100, "r1100"),
   (R1101, "r1101"),
   (R1102, "r1102"),
   (R1103, "r1103"),
   (R1104, "r1104"),
   (R1105, "r1105"),
   (R1106, "r1106"),
   (R1107, "r1107"),
   (R1110, "r1110"),
   (R1111, "r1111"),
   (R1112, "r1112"),
   (R1113, "r1113"),
   (R1114, "r1114"),
   (R1115, "r1115"),
   (R1116, "r1116"),
   (R1117, "r1117"),
   (R1120, "r1120"),
   (R1121, "r1121"),
   (R1122, "r1122"),
   (R1123, "r1123"),
   (R1124, "r1124"),
   (R1125, "r1125"),
   (R1126, "r1126"),
   (R1127, "r1127"),
   (R1130, "r1130"),
   (R1131, "r1131"),
   (R1132, "r1132"),
   (R1133, "r1133"),
   (R1134, "r1134"),
   (R1135, "r1135"),
   (R1136, "r1136"),
   (R1137, "r1137"),
   (R1140, "r1140"),
   (R1141, "r1141"),
   (R1142, "r1142"),
   (R1143, "r1143"),
   (R1144, "r1144"),
   (R1145, "r1145"),
   (R1146, "r1146"),
   (R1147, "r1147"),
   (R1150, "r1150"),
   (R1151, "r1151"),
   (R1152, "r1152"),
   (R1153, "r1153"),
   (R1154, "r1154"),
   (R1155, "r1155"),
   (R1156, "r1156"),
   (R1157, "r1157"),
   (R1160, "r1160"),
   (R1161, "r1161"),
   (R1162, "r1162"),
   (R1163, "r1163"),
   (R1164, "r1164"),
   (R1165, "r1165"),
   (R1166, "r1166"),
   (R1167, "r1167"),
   (R1170, "r1170"),
   (R1171, "r1171"),
   (R1172, "r1172"),
   (R1173, "r1173"),
   (R1174, "r1174"),
   (R1175, "r1175"),
   (R1176, "r1176"),
   (R1177, "r1177"),
   (R1200, "r1200"),
   (R1201, "r1201"),
   (R1202, "r1202"),
   (R1203, "r1203"),
   (R1204, "r1204"),
   (R1205, "r1205"),
   (R1206, "r1206"),
   (R1207, "r1207"),
   (R1210, "r1210"),
   (R1211, "r1211"),
   (R1212, "r1212"),
   (R1213, "r1213"),
   (R1214, "r1214"),
   (R1215, "r1215"),
   (R1216, "r1216"),
   (R1217, "r1217"),
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
   (AX_HI, "ax_hi"),
   (CX_HI, "cx_hi"),
   (DX_HI, "dx_hi"),
   (BX_HI, "bx_hi"),
   (SI_HI, "si_hi"),
   (DI_HI, "di_hi"),
   (SP_HI, "sp_hi"),
   (BP_HI, "bp_hi"),
   (R8W_HI, "r8w_hi"),
   (R9W_HI, "r9w_hi"),
   (R10W_HI, "r10w_hi"),
   (R11W_HI, "r11w_hi"),
   (R12W_HI, "r12w_hi"),
   (R13W_HI, "r13w_hi"),
   (R14W_HI, "r14w_hi"),
   (R15W_HI, "r15w_hi"),
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
   (EAX_HI, "eax_hi"),
   (ECX_HI, "ecx_hi"),
   (EDX_HI, "edx_hi"),
   (EBX_HI, "ebx_hi"),
   (ESI_HI, "esi_hi"),
   (EDI_HI, "edi_hi"),
   (ESP_HI, "esp_hi"),
   (EBP_HI, "ebp_hi"),
   (R8D_HI, "r8d_hi"),
   (R9D_HI, "r9d_hi"),
   (R10D_HI, "r10d_hi"),
   (R11D_HI, "r11d_hi"),
   (R12D_HI, "r12d_hi"),
   (R13D_HI, "r13d_hi"),
   (R14D_HI, "r14d_hi"),
   (R15D_HI, "r15d_hi"),
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
   (UMM0, "umm0"),
   (UMM1, "umm1"),
   (UMM2, "umm2"),
   (UMM3, "umm3"),
   (UMM4, "umm4"),
   (UMM5, "umm5"),
   (UMM6, "umm6"),
   (UMM7, "umm7"),
   (UMM8, "umm8"),
   (UMM9, "umm9"),
   (UMM10, "umm10"),
   (UMM11, "umm11"),
   (UMM12, "umm12"),
   (UMM13, "umm13"),
   (UMM14, "umm14"),
   (UMM15, "umm15"),
   (VMM0, "vmm0"),
   (VMM1, "vmm1"),
   (VMM2, "vmm2"),
   (VMM3, "vmm3"),
   (VMM4, "vmm4"),
   (VMM5, "vmm5"),
   (VMM6, "vmm6"),
   (VMM7, "vmm7"),
   (VMM8, "vmm8"),
   (VMM9, "vmm9"),
   (VMM10, "vmm10"),
   (VMM11, "vmm11"),
   (VMM12, "vmm12"),
   (VMM13, "vmm13"),
   (VMM14, "vmm14"),
   (VMM15, "vmm15"),
   (WMM0, "wmm0"),
   (WMM1, "wmm1"),
   (WMM2, "wmm2"),
   (WMM3, "wmm3"),
   (WMM4, "wmm4"),
   (WMM5, "wmm5"),
   (WMM6, "wmm6"),
   (WMM7, "wmm7"),
   (WMM8, "wmm8"),
   (WMM9, "wmm9"),
   (WMM10, "wmm10"),
   (WMM11, "wmm11"),
   (WMM12, "wmm12"),
   (WMM13, "wmm13"),
   (WMM14, "wmm14"),
   (WMM15, "wmm15"),
   (YMM0, "ymm0"),
   (YMM1, "ymm1"),
   (YMM2, "ymm2"),
   (YMM3, "ymm3"),
   (YMM4, "ymm4"),
   (YMM5, "ymm5"),
   (YMM6, "ymm6"),
   (YMM7, "ymm7"),
   (YMM8, "ymm8"),
   (YMM9, "ymm9"),
   (YMM10, "ymm10"),
   (YMM11, "ymm11"),
   (YMM12, "ymm12"),
   (YMM13, "ymm13"),
   (YMM14, "ymm14"),
   (YMM15, "ymm15"),
   (UMM0_HI, "umm0_hi"),
   (VMM0_HI, "vmm0_hi"),
   (VMM1_HI, "vmm1_hi"),
   (VMM2_HI, "vmm2_hi"),
   (VMM3_HI, "vmm3_hi"),
   (VMM4_HI, "vmm4_hi"),
   (VMM5_HI, "vmm5_hi"),
   (VMM6_HI, "vmm6_hi"),
   (VMM7_HI, "vmm7_hi"),
   (VMM8_HI, "vmm8_hi"),
   (VMM9_HI, "vmm9_hi"),
   (VMM10_HI, "vmm10_hi"),
   (VMM11_HI, "vmm11_hi"),
   (VMM12_HI, "vmm12_hi"),
   (VMM13_HI, "vmm13_hi"),
   (VMM14_HI, "vmm14_hi"),
   (VMM15_HI, "vmm15_hi"),
   (WMM0_HI, "wmm0_hi"),
   (WMM1_HI, "wmm1_hi"),
   (WMM2_HI, "wmm2_hi"),
   (WMM3_HI, "wmm3_hi"),
   (WMM4_HI, "wmm4_hi"),
   (WMM5_HI, "wmm5_hi"),
   (WMM6_HI, "wmm6_hi"),
   (WMM7_HI, "wmm7_hi"),
   (WMM8_HI, "wmm8_hi"),
   (WMM9_HI, "wmm9_hi"),
   (WMM10_HI, "wmm10_hi"),
   (WMM11_HI, "wmm11_hi"),
   (WMM12_HI, "wmm12_hi"),
   (WMM13_HI, "wmm13_hi"),
   (WMM14_HI, "wmm14_hi"),
   (WMM15_HI, "wmm15_hi"),
   (RCX_RDX, "rcx_rdx"),
   (RSI_RDI, "rsi_rdi"),
   (R8_R9, "r8_r9"),
   (R10_R11, "r10_r11"),
   (YMM1_15, "ymm1_15")]

data RegClassTriple = RegClassTriple {
  string   :: [Char],
  rc       :: X86RegisterClass,
  logSize  :: Int}

regClassTable =
  [RegClassTriple "gr8" GR8 1,
   RegClassTriple "gr8_norex" GR8_NOREX 1,
   RegClassTriple "gr16" GR16 2,
   RegClassTriple "gr32" GR32 3,
   RegClassTriple "gr32_norex" GR32_NOREX 3,
   RegClassTriple "gr32_NOAX" GR32_NOAX 3,
   RegClassTriple "gr32_abcd" GR32 3,
   RegClassTriple "gr64" GR64 4,
   RegClassTriple "gr64_abcd" GR64 4,
   RegClassTriple "gr64_norex" GR64 4,
   RegClassTriple "gr64_nosp" GR64_NOSP 4,
   RegClassTriple "gr64_norex_nosp" GR64_NOSP 4,
   RegClassTriple "gr64" Ptr_rc 4,
   RegClassTriple "gr64_nosp" Ptr_rc_nosp 4,
   RegClassTriple "gr64" Ptr_rc_norex 4,
   RegClassTriple "gr64_nosp" Ptr_rc_norex_nosp 4,
   RegClassTriple "gr64_tc" Ptr_rc_tailcall 4,
   RegClassTriple "gr64_with_sub_8bit" GR64 4,
   RegClassTriple "fr32" FR32 3,
   RegClassTriple "fr64" FR64 4,
   RegClassTriple "fr128" FR128 5,
   RegClassTriple "vr128" VR128 5,
   RegClassTriple "vr256" VR256 6]

regClassStringToLogSize = M.fromList $ nub $ sort $
  [(string r, logSize r) | r <- regClassTable]

regClassToLogSize = M.fromList $ nub $ sort $
  [(rc r, logSize r) | r <- regClassTable]

