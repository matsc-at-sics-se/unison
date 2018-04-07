-- This file has been generated by specsgen. Do not modify by hand!

module Unison.Target.X86.SpecsGen.Parent (parent) where
import Unison.Target.X86.SpecsGen.X86InstructionDecl
parent i
  | i `elem`
      [ADC16i16, ADC16mi, ADC16mi8, ADC16mr, ADC16ri, ADC16ri8, ADC16rm,
       ADC16rr, ADC16rr_REV, ADC32i32, ADC32mi, ADC32mi8, ADC32mr,
       ADC32ri, ADC32ri8, ADC32rm, ADC32rr, ADC32rr_REV, ADC64i32,
       ADC64mi32, ADC64mi8, ADC64mr, ADC64ri32, ADC64ri8, ADC64rm,
       ADC64rr, ADC64rr_REV, ADC8i8, ADC8mi, ADC8mi8, ADC8mr, ADC8ri,
       ADC8ri8, ADC8rm, ADC8rr, ADC8rr_REV, ADCX32rm, ADCX32rr, ADCX64rm,
       ADCX64rr, ADD16i16, ADD16mi, ADD16mi8, ADD16mr, ADD16ri, ADD16ri8,
       ADD16ri8_DB, ADD16ri_DB, ADD16rm, ADD16rr, ADD16rr_DB, ADD16rr_REV,
       ADD32i32, ADD32mi, ADD32mi8, ADD32mr, ADD32ri, ADD32ri8,
       ADD32ri8_DB, ADD32ri_DB, ADD32rm, ADD32rr, ADD32rr_DB, ADD32rr_REV,
       ADD64i32, ADD64mi32, ADD64mi8, ADD64mr, ADD64ri32, ADD64ri32_DB,
       ADD64ri8, ADD64ri8_DB, ADD64rm, ADD64rr, ADD64rr_DB, ADD64rr_REV,
       ADD8i8, ADD8mi, ADD8mi8, ADD8mr, ADD8ri, ADD8ri8, ADD8rm, ADD8rr,
       ADD8rr_REV, ADDPDrm, ADDPDrr, ADDPSrm, ADDPSrr, ADDSDrm, ADDSDrr,
       ADDSSrm, ADDSSrr, ADDSUBPDrm, ADDSUBPDrr, ADDSUBPSrm, ADDSUBPSrr,
       ADJCALLSTACKDOWN32, ADJCALLSTACKDOWN64, ADJCALLSTACKUP32,
       ADJCALLSTACKUP64, AND16i16, AND16mi, AND16mi8, AND16mr, AND16ri,
       AND16ri8, AND16rm, AND16rr, AND16rr_REV, AND32i32, AND32mi,
       AND32mi8, AND32mr, AND32ri, AND32ri8, AND32rm, AND32rr,
       AND32rr_REV, AND64i32, AND64mi32, AND64mi8, AND64mr, AND64ri32,
       AND64ri8, AND64rm, AND64rr, AND64rr_REV, AND8i8, AND8mi, AND8mi8,
       AND8mr, AND8ri, AND8ri8, AND8rm, AND8rr, AND8rr_REV, ANDN32rm,
       ANDN32rr, ANDN64rm, ANDN64rr, ANDNPDrm, ANDNPDrr, ANDNPSrm,
       ANDNPSrr, ANDPDrm, ANDPDrr, ANDPSrm, ANDPSrr, BSF16rm, BSF16rr,
       BSF32rm, BSF32rr, BSF64rm, BSF64rr, BSR16rm, BSR16rr, BSR32rm,
       BSR32rr, BSR64rm, BSR64rr, BSWAP32r, BSWAP64r, BT16mi8, BT16mr,
       BT16ri8, BT16rr, BT32mi8, BT32mr, BT32ri8, BT32rr, BT64mi8, BT64mr,
       BT64ri8, BT64rr, BTC16mi8, BTC16mr, BTC16ri8, BTC16rr, BTC32mi8,
       BTC32mr, BTC32ri8, BTC32rr, BTC64mi8, BTC64mr, BTC64ri8, BTC64rr,
       BTR16mi8, BTR16mr, BTR16ri8, BTR16rr, BTR32mi8, BTR32mr, BTR32ri8,
       BTR32rr, BTR64mi8, BTR64mr, BTR64ri8, BTR64rr, BTS16mi8, BTS16mr,
       BTS16ri8, BTS16rr, BTS32mi8, BTS32mr, BTS32ri8, BTS32rr, BTS64mi8,
       BTS64mr, BTS64ri8, BTS64rr, CALL16m, CALL16r, CALL32m, CALL32r,
       CALL64m, CALL64pcrel32, CALL64r, CALLpcrel16, CALLpcrel32, CBW,
       CDQ, CDQE, CLC, CLD, CMC, CMOVA16rm, CMOVA16rr, CMOVA32rm,
       CMOVA32rr, CMOVA64rm, CMOVA64rr, CMOVAE16rm, CMOVAE16rr,
       CMOVAE32rm, CMOVAE32rr, CMOVAE64rm, CMOVAE64rr, CMOVB16rm,
       CMOVB16rr, CMOVB32rm, CMOVB32rr, CMOVB64rm, CMOVB64rr, CMOVBE16rm,
       CMOVBE16rr, CMOVBE32rm, CMOVBE32rr, CMOVBE64rm, CMOVBE64rr,
       CMOVE16rm, CMOVE16rr, CMOVE32rm, CMOVE32rr, CMOVE64rm, CMOVE64rr,
       CMOVG16rm, CMOVG16rr, CMOVG32rm, CMOVG32rr, CMOVG64rm, CMOVG64rr,
       CMOVGE16rm, CMOVGE16rr, CMOVGE32rm, CMOVGE32rr, CMOVGE64rm,
       CMOVGE64rr, CMOVL16rm, CMOVL16rr, CMOVL32rm, CMOVL32rr, CMOVL64rm,
       CMOVL64rr, CMOVLE16rm, CMOVLE16rr, CMOVLE32rm, CMOVLE32rr,
       CMOVLE64rm, CMOVLE64rr, CMOVNE16rm, CMOVNE16rr, CMOVNE32rm,
       CMOVNE32rr, CMOVNE64rm, CMOVNE64rr, CMOVNO16rm, CMOVNO16rr,
       CMOVNO32rm, CMOVNO32rr, CMOVNO64rm, CMOVNO64rr, CMOVNP16rm,
       CMOVNP16rr, CMOVNP32rm, CMOVNP32rr, CMOVNP64rm, CMOVNP64rr,
       CMOVNS16rm, CMOVNS16rr, CMOVNS32rm, CMOVNS32rr, CMOVNS64rm,
       CMOVNS64rr, CMOVO16rm, CMOVO16rr, CMOVO32rm, CMOVO32rr, CMOVO64rm,
       CMOVO64rr, CMOVP16rm, CMOVP16rr, CMOVP32rm, CMOVP32rr, CMOVP64rm,
       CMOVP64rr, CMOVS16rm, CMOVS16rr, CMOVS32rm, CMOVS32rr, CMOVS64rm,
       CMOVS64rr, CMP16i16, CMP16mi, CMP16mi8, CMP16mr, CMP16ri, CMP16ri8,
       CMP16rm, CMP16rr, CMP16rr_REV, CMP32i32, CMP32mi, CMP32mi8,
       CMP32mr, CMP32ri, CMP32ri8, CMP32rm, CMP32rr, CMP32rr_REV,
       CMP64i32, CMP64mi32, CMP64mi8, CMP64mr, CMP64ri32, CMP64ri8,
       CMP64rm, CMP64rr, CMP64rr_REV, CMP8i8, CMP8mi, CMP8mi8, CMP8mr,
       CMP8ri, CMP8ri8, CMP8rm, CMP8rr, CMP8rr_REV, CMPPDrmi,
       CMPPDrmi_alt, CMPPDrri, CMPPDrri_alt, CMPPSrmi, CMPPSrmi_alt,
       CMPPSrri, CMPPSrri_alt, CMPSDrm, CMPSDrm_alt, CMPSDrr, CMPSDrr_alt,
       CMPSSrm, CMPSSrm_alt, CMPSSrr, CMPSSrr_alt, CMPXCHG16B,
       CMPXCHG16rm, CMPXCHG16rr, CMPXCHG32rm, CMPXCHG32rr, CMPXCHG64rm,
       CMPXCHG64rr, CMPXCHG8B, CMPXCHG8rm, CMPXCHG8rr, COMISDrm, COMISDrr,
       COMISSrm, COMISSrr, CQO, CVTDQ2PDrm, CVTDQ2PDrr, CVTDQ2PSrm,
       CVTDQ2PSrr, CVTPD2DQrm, CVTPD2DQrr, CVTPD2PSrm, CVTPD2PSrr,
       CVTPS2DQrm, CVTPS2DQrr, CVTPS2PDrm, CVTPS2PDrr, CVTSD2SI64rr,
       CVTSD2SIrr, CVTSD2SSrm, CVTSD2SSrr, CVTSI2SD64rm, CVTSI2SD64rr,
       CVTSI2SDrm, CVTSI2SDrr, CVTSI2SS64rm, CVTSI2SS64rr, CVTSI2SSrm,
       CVTSI2SSrr, CVTSS2SDrm, CVTSS2SDrr, CVTSS2SI64rr, CVTSS2SIrr,
       CVTTPD2DQrm, CVTTPD2DQrr, CVTTPS2DQrm, CVTTPS2DQrr, CVTTSD2SI64rr,
       CVTTSD2SIrr, CVTTSS2SI64rr, CVTTSS2SIrr, CWD, CWDE, DEC16m, DEC16r,
       DEC16r_alt, DEC32m, DEC32r, DEC32r_alt, DEC64m, DEC64r, DEC8m,
       DEC8r, DIV16m, DIV16r, DIV32m, DIV32r, DIV64m, DIV64r, DIV8m,
       DIV8r, DIVPDrm, DIVPDrr, DIVPSrm, DIVPSrr, DIVSDrm, DIVSDrr,
       DIVSSrm, DIVSSrr, DPPDrmi, DPPDrri, DPPSrmi, DPPSrri, ENTER,
       FNSTCW16m, FsANDNPDrm, FsANDNPDrr, FsANDNPSrm, FsANDNPSrr,
       FsANDPDrm, FsANDPDrr, FsANDPSrm, FsANDPSrr, FsFLD0SD, FsFLD0SS,
       FsMOVAPDrm, FsMOVAPSrm, FsORPDrm, FsORPDrr, FsORPSrm, FsORPSrr,
       FsVMOVAPDrm, FsVMOVAPSrm, FsXORPDrm, FsXORPDrr, FsXORPSrm,
       FsXORPSrr, FvANDNPDrm, FvANDNPDrr, FvANDNPSrm, FvANDNPSrr,
       FvANDPDrm, FvANDPDrr, FvANDPSrm, FvANDPSrr, FvORPDrm, FvORPDrr,
       FvORPSrm, FvORPSrr, FvXORPDrm, FvXORPDrr, FvXORPSrm, FvXORPSrr,
       IDIV16m, IDIV16r, IDIV32m, IDIV32r, IDIV64m, IDIV64r, IDIV8m,
       IDIV8r, IMUL16m, IMUL16r, IMUL16rm, IMUL16rmi, IMUL16rmi8,
       IMUL16rr, IMUL16rri, IMUL16rri8, IMUL32m, IMUL32r, IMUL32rm,
       IMUL32rmi, IMUL32rmi8, IMUL32rr, IMUL32rri, IMUL32rri8, IMUL64m,
       IMUL64r, IMUL64rm, IMUL64rmi32, IMUL64rmi8, IMUL64rr, IMUL64rri32,
       IMUL64rri8, IMUL8m, IMUL8r, INC16m, INC16r, INC16r_alt, INC32m,
       INC32r, INC32r_alt, INC64m, INC64r, INC8m, INC8r, JAE_1, JAE_2,
       JAE_4, JA_1, JA_2, JA_4, JBE_1, JBE_2, JBE_4, JB_1, JB_2, JB_4,
       JCXZ, JECXZ, JE_1, JE_2, JE_4, JGE_1, JGE_2, JGE_4, JG_1, JG_2,
       JG_4, JLE_1, JLE_2, JLE_4, JL_1, JL_2, JL_4, JMP16m, JMP16r,
       JMP32m, JMP32r, JMP64m, JMP64r, JMP_1, JMP_2, JMP_4, JNE_1, JNE_2,
       JNE_4, JNO_1, JNO_2, JNO_4, JNP_1, JNP_2, JNP_4, JNS_1, JNS_2,
       JNS_4, JO_1, JO_2, JO_4, JP_1, JP_2, JP_4, JRCXZ, JS_1, JS_2, JS_4,
       LEA16r, LEA32r, LEA64_32r, LEA64r, LEAVE, LEAVE64, LODSB, LODSL,
       LODSQ, LODSW, LOOP, LOOPE, LOOPNE, MAXCPDrm, MAXCPDrr, MAXCPSrm,
       MAXCPSrr, MAXCSDrm, MAXCSDrr, MAXCSSrm, MAXCSSrr, MAXPDrm, MAXPDrr,
       MAXPSrm, MAXPSrr, MAXSDrm, MAXSDrr, MAXSSrm, MAXSSrr, MINCPDrm,
       MINCPDrr, MINCPSrm, MINCPSrr, MINCSDrm, MINCSDrr, MINCSSrm,
       MINCSSrr, MINPDrm, MINPDrr, MINPSrm, MINPSrr, MINSDrm, MINSDrr,
       MINSSrm, MINSSrr, MOV16ao16, MOV16ao32, MOV16ao64, MOV16mi,
       MOV16mr, MOV16o16a, MOV16o32a, MOV16o64a, MOV16ri, MOV16ri_alt,
       MOV16rm, MOV16rr, MOV16rr_REV, MOV32ao16, MOV32ao32, MOV32ao64,
       MOV32mi, MOV32mr, MOV32o16a, MOV32o32a, MOV32o64a, MOV32r0,
       MOV32r1, MOV32r_1, MOV32ri, MOV32ri64, MOV32ri_alt, MOV32rm,
       MOV32rr, MOV32rr_REV, MOV64ao32, MOV64ao64, MOV64mi32, MOV64mr,
       MOV64o32a, MOV64o64a, MOV64ri, MOV64ri32, MOV64rm, MOV64rr,
       MOV64rr_REV, MOV64toPQIrm, MOV64toPQIrr, MOV64toSDrm, MOV64toSDrr,
       MOV8ao16, MOV8ao32, MOV8ao64, MOV8mi, MOV8mr, MOV8mr_NOREX,
       MOV8o16a, MOV8o32a, MOV8o64a, MOV8ri, MOV8ri_alt, MOV8rm,
       MOV8rm_NOREX, MOV8rr, MOV8rr_NOREX, MOV8rr_REV, MOVAPDmr, MOVAPDrm,
       MOVAPDrr, MOVAPDrr_REV, MOVAPSmr, MOVAPSrm, MOVAPSrr, MOVAPSrr_REV,
       MOVBE16mr, MOVBE16rm, MOVBE32mr, MOVBE32rm, MOVBE64mr, MOVBE64rm,
       MOVDDUPrm, MOVDDUPrr, MOVDI2PDIrm, MOVDI2PDIrr, MOVDI2SSrm,
       MOVDI2SSrr, MOVDQAmr, MOVDQArm, MOVDQArr, MOVDQArr_REV, MOVDQUmr,
       MOVDQUrm, MOVDQUrr, MOVDQUrr_REV, MOVHLPSrr, MOVHPDmr, MOVHPDrm,
       MOVHPSmr, MOVHPSrm, MOVLHPSrr, MOVLPDmr, MOVLPDrm, MOVLPSmr,
       MOVLPSrm, MOVNTDQArm, MOVNTDQmr, MOVNTPDmr, MOVNTPSmr, MOVNTSD,
       MOVNTSS, MOVPDI2DImr, MOVPDI2DIrr, MOVPQI2QImr, MOVPQI2QIrr,
       MOVPQIto64rm, MOVPQIto64rr, MOVQI2PQIrm, MOVSB, MOVSDmr, MOVSDrm,
       MOVSDrr, MOVSDrr_REV, MOVSDto64mr, MOVSDto64rr, MOVSHDUPrm,
       MOVSHDUPrr, MOVSL, MOVSLDUPrm, MOVSLDUPrr, MOVSQ, MOVSS2DImr,
       MOVSS2DIrr, MOVSSmr, MOVSSrm, MOVSSrr, MOVSSrr_REV, MOVSW,
       MOVSX16rm8, MOVSX16rr8, MOVSX32_NOREXrm8, MOVSX32_NOREXrr8,
       MOVSX32rm16, MOVSX32rm8, MOVSX32rr16, MOVSX32rr8, MOVSX64rm16,
       MOVSX64rm32, MOVSX64rm8, MOVSX64rr16, MOVSX64rr32, MOVSX64rr8,
       MOVUPDmr, MOVUPDrm, MOVUPDrr, MOVUPDrr_REV, MOVUPSmr, MOVUPSrm,
       MOVUPSrr, MOVUPSrr_REV, MOVZPQILo2PQIrm, MOVZPQILo2PQIrr,
       MOVZQI2PQIrm, MOVZX16rm8, MOVZX16rr8, MOVZX32_NOREXrm8,
       MOVZX32_NOREXrr8, MOVZX32rm16, MOVZX32rm8, MOVZX32rr16, MOVZX32rr8,
       MOVZX64rm16, MOVZX64rm8, MOVZX64rr16, MOVZX64rr8, MPSADBWrmi,
       MPSADBWrri, MUL16m, MUL16r, MUL32m, MUL32r, MUL64m, MUL64r, MUL8m,
       MUL8r, MULPDrm, MULPDrr, MULPSrm, MULPSrr, MULSDrm, MULSDrr,
       MULSSrm, MULSSrr, NEG16m, NEG16r, NEG32m, NEG32r, NEG64m, NEG64r,
       NEG8m, NEG8r, NOOP, NOOPL, NOOPW, NOT16m, NOT16r, NOT32m, NOT32r,
       NOT64m, NOT64r, NOT8m, NOT8r, OR16i16, OR16mi, OR16mi8, OR16mr,
       OR16ri, OR16ri8, OR16rm, OR16rr, OR16rr_REV, OR32i32, OR32mi,
       OR32mi8, OR32mr, OR32mrLocked, OR32ri, OR32ri8, OR32rm, OR32rr,
       OR32rr_REV, OR64i32, OR64mi32, OR64mi8, OR64mr, OR64ri32, OR64ri8,
       OR64rm, OR64rr, OR64rr_REV, OR8i8, OR8mi, OR8mi8, OR8mr, OR8ri,
       OR8ri8, OR8rm, OR8rr, OR8rr_REV, ORPDrm, ORPDrr, ORPSrm, ORPSrr,
       PACKSSDWrm, PACKSSDWrr, PACKSSWBrm, PACKSSWBrr, PACKUSDWrm,
       PACKUSDWrr, PACKUSWBrm, PACKUSWBrr, PADDBrm, PADDBrr, PADDDrm,
       PADDDrr, PADDQrm, PADDQrr, PADDWrm, PADDWrr, PANDNrm, PANDNrr,
       PANDrm, PANDrr, PCMPEQBrm, PCMPEQBrr, PCMPEQDrm, PCMPEQDrr,
       PCMPEQQrm, PCMPEQQrr, PCMPEQWrm, PCMPEQWrr, PCMPGTBrm, PCMPGTBrr,
       PCMPGTDrm, PCMPGTDrr, PCMPGTQrm, PCMPGTQrr, PCMPGTWrm, PCMPGTWrr,
       PMULDQrm, PMULDQrr, PMULUDQrm, PMULUDQrr, POP16r, POP16rmm,
       POP16rmr, POP32r, POP32rmm, POP32rmr, POP64r, POP64rmm, POP64rmr,
       POPCNT16rm, POPCNT16rr, POPCNT32rm, POPCNT32rr, POPCNT64rm,
       POPCNT64rr, PORrm, PORrr, PSHUFBrm, PSHUFBrr, PSHUFDmi, PSHUFDri,
       PSHUFHWmi, PSHUFHWri, PSHUFLWmi, PSHUFLWri, PSLLDQri, PSLLDri,
       PSLLDrm, PSLLDrr, PSLLQri, PSLLQrm, PSLLQrr, PSLLWri, PSLLWrm,
       PSLLWrr, PSRADri, PSRADrm, PSRADrr, PSRAWri, PSRAWrm, PSRAWrr,
       PSRLDQri, PSRLDri, PSRLDrm, PSRLDrr, PSRLQri, PSRLQrm, PSRLQrr,
       PSRLWri, PSRLWrm, PSRLWrr, PSUBBrm, PSUBBrr, PSUBDrm, PSUBDrr,
       PSUBQrm, PSUBQrr, PSUBWrm, PSUBWrr, PUNPCKHBWrm, PUNPCKHBWrr,
       PUNPCKHDQrm, PUNPCKHDQrr, PUNPCKHQDQrm, PUNPCKHQDQrr, PUNPCKHWDrm,
       PUNPCKHWDrr, PUNPCKLBWrm, PUNPCKLBWrr, PUNPCKLDQrm, PUNPCKLDQrr,
       PUNPCKLQDQrm, PUNPCKLQDQrr, PUNPCKLWDrm, PUNPCKLWDrr, PUSH16i8,
       PUSH16r, PUSH16rmm, PUSH16rmr, PUSH32i8, PUSH32r, PUSH32rmm,
       PUSH32rmr, PUSH64i32, PUSH64i8, PUSH64r, PUSH64rmm, PUSH64rmr,
       PUSHA16, PUSHA32, PUSHCS16, PUSHCS32, PUSHDS16, PUSHDS32, PUSHES16,
       PUSHES32, PUSHF16, PUSHF32, PUSHF64, PUSHFS16, PUSHFS32, PUSHFS64,
       PUSHGS16, PUSHGS32, PUSHGS64, PUSHSS16, PUSHSS32, PUSHi16, PUSHi32,
       PXORrm, PXORrr, RCL16m1, RCL16mCL, RCL16mi, RCL16r1, RCL16rCL,
       RCL16ri, RCL32m1, RCL32mCL, RCL32mi, RCL32r1, RCL32rCL, RCL32ri,
       RCL64m1, RCL64mCL, RCL64mi, RCL64r1, RCL64rCL, RCL64ri, RCL8m1,
       RCL8mCL, RCL8mi, RCL8r1, RCL8rCL, RCL8ri, RCR16m1, RCR16mCL,
       RCR16mi, RCR16r1, RCR16rCL, RCR16ri, RCR32m1, RCR32mCL, RCR32mi,
       RCR32r1, RCR32rCL, RCR32ri, RCR64m1, RCR64mCL, RCR64mi, RCR64r1,
       RCR64rCL, RCR64ri, RCR8m1, RCR8mCL, RCR8mi, RCR8r1, RCR8rCL,
       RCR8ri, REPNE_PREFIX, REP_MOVSB_32, REP_MOVSB_64, REP_MOVSD_32,
       REP_MOVSD_64, REP_MOVSQ_64, REP_MOVSW_32, REP_MOVSW_64, REP_PREFIX,
       REP_STOSB_32, REP_STOSB_64, REP_STOSD_32, REP_STOSD_64,
       REP_STOSQ_64, REP_STOSW_32, REP_STOSW_64, RETIL, RETIQ, RETIW,
       RETL, RETQ, RETW, REX64_PREFIX, ROL16m1, ROL16mCL, ROL16mi,
       ROL16r1, ROL16rCL, ROL16ri, ROL32m1, ROL32mCL, ROL32mi, ROL32r1,
       ROL32rCL, ROL32ri, ROL64m1, ROL64mCL, ROL64mi, ROL64r1, ROL64rCL,
       ROL64ri, ROL8m1, ROL8mCL, ROL8mi, ROL8r1, ROL8rCL, ROL8ri, ROR16m1,
       ROR16mCL, ROR16mi, ROR16r1, ROR16rCL, ROR16ri, ROR32m1, ROR32mCL,
       ROR32mi, ROR32r1, ROR32rCL, ROR32ri, ROR64m1, ROR64mCL, ROR64mi,
       ROR64r1, ROR64rCL, ROR64ri, ROR8m1, ROR8mCL, ROR8mi, ROR8r1,
       ROR8rCL, ROR8ri, RORX32mi, RORX32ri, RORX64mi, RORX64ri, ROUNDPDm,
       ROUNDPDr, ROUNDPSm, ROUNDPSr, ROUNDSDm, ROUNDSDr, ROUNDSSm,
       ROUNDSSr, SAR16m1, SAR16mCL, SAR16mi, SAR16r1, SAR16rCL, SAR16ri,
       SAR32m1, SAR32mCL, SAR32mi, SAR32r1, SAR32rCL, SAR32ri, SAR64m1,
       SAR64mCL, SAR64mi, SAR64r1, SAR64rCL, SAR64ri, SAR8m1, SAR8mCL,
       SAR8mi, SAR8r1, SAR8rCL, SAR8ri, SARX32rm, SARX32rr, SARX64rm,
       SARX64rr, SBB16i16, SBB16mi, SBB16mi8, SBB16mr, SBB16ri, SBB16ri8,
       SBB16rm, SBB16rr, SBB16rr_REV, SBB32i32, SBB32mi, SBB32mi8,
       SBB32mr, SBB32ri, SBB32ri8, SBB32rm, SBB32rr, SBB32rr_REV,
       SBB64i32, SBB64mi32, SBB64mi8, SBB64mr, SBB64ri32, SBB64ri8,
       SBB64rm, SBB64rr, SBB64rr_REV, SBB8i8, SBB8mi, SBB8mi8, SBB8mr,
       SBB8ri, SBB8ri8, SBB8rm, SBB8rr, SBB8rr_REV, SETAEm, SETAEr, SETAm,
       SETAr, SETBEm, SETBEr, SETB_C16r, SETB_C32r, SETB_C64r, SETB_C8r,
       SETBm, SETBr, SETEm, SETEr, SETGEm, SETGEr, SETGm, SETGr, SETLEm,
       SETLEr, SETLm, SETLr, SETNEm, SETNEr, SETNOm, SETNOr, SETNPm,
       SETNPr, SETNSm, SETNSr, SETOm, SETOr, SETPm, SETPr, SETSm, SETSr,
       SHL16m1, SHL16mCL, SHL16mi, SHL16r1, SHL16rCL, SHL16ri, SHL32m1,
       SHL32mCL, SHL32mi, SHL32r1, SHL32rCL, SHL32ri, SHL64m1, SHL64mCL,
       SHL64mi, SHL64r1, SHL64rCL, SHL64ri, SHL8m1, SHL8mCL, SHL8mi,
       SHL8r1, SHL8rCL, SHL8ri, SHLD16mrCL, SHLD16mri8, SHLD16rrCL,
       SHLD16rri8, SHLD32mrCL, SHLD32mri8, SHLD32rrCL, SHLD32rri8,
       SHLD64mrCL, SHLD64mri8, SHLD64rrCL, SHLD64rri8, SHLX32rm, SHLX32rr,
       SHLX64rm, SHLX64rr, SHR16m1, SHR16mCL, SHR16mi, SHR16r1, SHR16rCL,
       SHR16ri, SHR32m1, SHR32mCL, SHR32mi, SHR32r1, SHR32rCL, SHR32ri,
       SHR64m1, SHR64mCL, SHR64mi, SHR64r1, SHR64rCL, SHR64ri, SHR8m1,
       SHR8mCL, SHR8mi, SHR8r1, SHR8rCL, SHR8ri, SHRD16mrCL, SHRD16mri8,
       SHRD16rrCL, SHRD16rri8, SHRD32mrCL, SHRD32mri8, SHRD32rrCL,
       SHRD32rri8, SHRD64mrCL, SHRD64mri8, SHRD64rrCL, SHRD64rri8,
       SHRX32rm, SHRX32rr, SHRX64rm, SHRX64rr, SQRTSDm, SQRTSDr, SQRTSSm,
       SQRTSSr, STC, STD, STOSB, STOSL, STOSQ, STOSW, SUB16i16, SUB16mi,
       SUB16mi8, SUB16mr, SUB16ri, SUB16ri8, SUB16rm, SUB16rr,
       SUB16rr_REV, SUB32i32, SUB32mi, SUB32mi8, SUB32mr, SUB32ri,
       SUB32ri8, SUB32rm, SUB32rr, SUB32rr_REV, SUB64i32, SUB64mi32,
       SUB64mi8, SUB64mr, SUB64ri32, SUB64ri8, SUB64rm, SUB64rr,
       SUB64rr_REV, SUB8i8, SUB8mi, SUB8mi8, SUB8mr, SUB8ri, SUB8ri8,
       SUB8rm, SUB8rr, SUB8rr_REV, SUBPDrm, SUBPDrr, SUBPSrm, SUBPSrr,
       SUBSDrm, SUBSDrr, SUBSSrm, SUBSSrr, TAILJMPd, TAILJMPd64,
       TAILJMPd64_REX, TAILJMPm, TAILJMPm64, TAILJMPm64_REX, TAILJMPr,
       TAILJMPr64, TAILJMPr64_REX, TCRETURNdi, TCRETURNdi64, TCRETURNmi,
       TCRETURNmi64, TCRETURNri, TCRETURNri64, TEST16i16, TEST16mi,
       TEST16ri, TEST16rm, TEST16rr, TEST32i32, TEST32mi, TEST32ri,
       TEST32rm, TEST32rr, TEST64i32, TEST64mi32, TEST64ri32, TEST64rm,
       TEST64rr, TEST8i8, TEST8mi, TEST8ri, TEST8ri_NOREX, TEST8rm,
       TEST8rr, UCOMISDrm, UCOMISDrr, UCOMISSrm, UCOMISSrr, UD2B, V_SET0,
       V_SETALLONES, XCHG16ar, XCHG16rm, XCHG16rr, XCHG32ar, XCHG32ar64,
       XCHG32rm, XCHG32rr, XCHG64ar, XCHG64rm, XCHG64rr, XCHG8rm, XCHG8rr,
       XOR16i16, XOR16mi, XOR16mi8, XOR16mr, XOR16ri, XOR16ri8, XOR16rm,
       XOR16rr, XOR16rr_REV, XOR32i32, XOR32mi, XOR32mi8, XOR32mr,
       XOR32ri, XOR32ri8, XOR32rm, XOR32rr, XOR32rr_REV, XOR64i32,
       XOR64mi32, XOR64mi8, XOR64mr, XOR64ri32, XOR64ri8, XOR64rm,
       XOR64rr, XOR64rr_REV, XOR8i8, XOR8mi, XOR8mi8, XOR8mr, XOR8ri,
       XOR8ri8, XOR8rm, XOR8rr, XOR8rr_REV, XORPDrm, XORPDrr, XORPSrm,
       XORPSrr]
    = Nothing
  | i `elem` [MOV16ri_demat, MOV16ri_remat, MOV16ri_source] =
    Just MOV16ri
  | i `elem`
      [MOV16ri_alt_demat, MOV16ri_alt_remat, MOV16ri_alt_source]
    = Just MOV16ri_alt
  | i `elem` [MOVE16] = Just MOV16rr
  | i `elem` [MOV32r0_demat, MOV32r0_remat, MOV32r0_source] =
    Just MOV32r0
  | i `elem` [MOV32r1_demat, MOV32r1_remat, MOV32r1_source] =
    Just MOV32r1
  | i `elem` [MOV32r_1_demat, MOV32r_1_remat, MOV32r_1_source] =
    Just MOV32r_1
  | i `elem` [MOV32ri_demat, MOV32ri_remat, MOV32ri_source] =
    Just MOV32ri
  | i `elem` [MOV32ri64_demat, MOV32ri64_remat, MOV32ri64_source] =
    Just MOV32ri64
  | i `elem`
      [MOV32ri_alt_demat, MOV32ri_alt_remat, MOV32ri_alt_source]
    = Just MOV32ri_alt
  | i `elem` [MOVE32] = Just MOV32rr
  | i `elem` [MOV64ri_demat, MOV64ri_remat, MOV64ri_source] =
    Just MOV64ri
  | i `elem` [MOV64ri32_demat, MOV64ri32_remat, MOV64ri32_source] =
    Just MOV64ri32
  | i `elem` [MOVE64] = Just MOV64rr
  | i `elem` [MOV8ri_demat, MOV8ri_remat, MOV8ri_source] =
    Just MOV8ri
  | i `elem` [MOVE8] = Just MOV8rr

