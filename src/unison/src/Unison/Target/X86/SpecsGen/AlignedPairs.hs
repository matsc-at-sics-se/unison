-- This file has been generated by specsgen. Do not modify by hand!

module Unison.Target.X86.SpecsGen.AlignedPairs (alignedPairs) where
import Unison.Target.X86.SpecsGen.X86InstructionDecl
alignedPairs i ([], [])
  | i `elem`
      [CBW, CDQ, CDQE, CLC, CLD, CMC, CQO, CWD, CWDE, LEAVE, LEAVE64,
       NOOP, PUSHA16, PUSHA32, PUSHCS16, PUSHCS32, PUSHDS16, PUSHDS32,
       PUSHES16, PUSHES32, PUSHF16, PUSHF32, PUSHF64, PUSHFS16, PUSHFS32,
       PUSHFS64, PUSHGS16, PUSHGS32, PUSHGS64, PUSHSS16, PUSHSS32,
       REPNE_PREFIX, REP_MOVSB_32, REP_MOVSB_64, REP_MOVSD_32,
       REP_MOVSD_64, REP_MOVSQ_64, REP_MOVSW_32, REP_MOVSW_64, REP_PREFIX,
       REP_STOSB_32, REP_STOSB_64, REP_STOSD_32, REP_STOSD_64,
       REP_STOSQ_64, REP_STOSW_32, REP_STOSW_64, RETW, REX64_PREFIX, STC,
       STD, UD2B]
    = []
alignedPairs i ([], [_])
  | i `elem`
      [MOV32r0, MOV32r0_source, MOV32r1, MOV32r1_source, MOV32r_1,
       MOV32r_1_source, SETAEr, SETAr, SETBEr, SETB_C16r, SETB_C32r,
       SETB_C64r, SETB_C8r, SETBr, SETEr, SETGEr, SETGr, SETLEr, SETLr,
       SETNEr, SETNOr, SETNPr, SETNSr, SETOr, SETPr, SETSr, STOSB, STOSL,
       STOSQ, STOSW]
    = []
alignedPairs i ([], [_, _])
  | i `elem`
      [MOV16o16a, MOV16o32a, MOV16o64a, MOV32o16a, MOV32o32a, MOV32o64a,
       MOV64o32a, MOV64o64a, MOV8o16a, MOV8o32a, MOV8o64a]
    = []
alignedPairs i ([], [_])
  | i `elem` [POP16r, POP16rmr, POP32r, POP32rmr, POP64r, POP64rmr] =
    []
alignedPairs i ([_], []) | i `elem` [RETIW] = []
alignedPairs i ([_, _], []) | i `elem` [RETIL, RETIQ] = []
alignedPairs i ([_, _], [])
  | i `elem`
      [ADJCALLSTACKDOWN32, ADJCALLSTACKDOWN64, ADJCALLSTACKUP32,
       ADJCALLSTACKUP64]
    = []
alignedPairs i ([_], [])
  | i `elem`
      [CALL16r, CALL32r, CALL64pcrel32, CALL64r, CALLpcrel16,
       CALLpcrel32, JAE_1, JAE_2, JAE_4, JA_1, JA_2, JA_4, JBE_1, JBE_2,
       JBE_4, JB_1, JB_2, JB_4, JCXZ, JECXZ, JE_1, JE_2, JE_4, JGE_1,
       JGE_2, JGE_4, JG_1, JG_2, JG_4, JLE_1, JLE_2, JLE_4, JL_1, JL_2,
       JL_4, JMP16r, JMP32r, JMP64r, JMP_1, JMP_2, JMP_4, JNE_1, JNE_2,
       JNE_4, JNO_1, JNO_2, JNO_4, JNP_1, JNP_2, JNP_4, JNS_1, JNS_2,
       JNS_4, JO_1, JO_2, JO_4, JP_1, JP_2, JP_4, JRCXZ, JS_1, JS_2, JS_4,
       LOOP, LOOPE, LOOPNE, TAILJMPd, TAILJMPd64, TAILJMPd64_REX,
       TAILJMPr, TAILJMPr64, TAILJMPr64_REX]
    = []
alignedPairs i ([_, _], [])
  | i `elem` [TCRETURNdi, TCRETURNdi64, TCRETURNri, TCRETURNri64] =
    []
alignedPairs i ([_, _, _, _, _], [])
  | i `elem`
      [CALL16m, CALL32m, CALL64m, CMPXCHG16B, CMPXCHG8B, DEC16m, DEC32m,
       DEC64m, DEC8m, INC16m, INC32m, INC64m, INC8m, JMP16m, JMP32m,
       JMP64m, NEG16m, NEG32m, NEG64m, NEG8m, NOT16m, NOT32m, NOT64m,
       NOT8m, POP16rmm, POP32rmm, POP64rmm, RCL16m1, RCL16mCL, RCL32m1,
       RCL32mCL, RCL64m1, RCL64mCL, RCL8m1, RCL8mCL, RCR16m1, RCR16mCL,
       RCR32m1, RCR32mCL, RCR64m1, RCR64mCL, RCR8m1, RCR8mCL, ROL16m1,
       ROL16mCL, ROL32m1, ROL32mCL, ROL64m1, ROL64mCL, ROL8m1, ROL8mCL,
       ROR16m1, ROR16mCL, ROR32m1, ROR32mCL, ROR64m1, ROR64mCL, ROR8m1,
       ROR8mCL, SAR16m1, SAR16mCL, SAR32m1, SAR32mCL, SAR64m1, SAR64mCL,
       SAR8m1, SAR8mCL, SETAEm, SETAm, SETBEm, SETBm, SETEm, SETGEm,
       SETGm, SETLEm, SETLm, SETNEm, SETNOm, SETNPm, SETNSm, SETOm, SETPm,
       SETSm, SHL16m1, SHL16mCL, SHL32m1, SHL32mCL, SHL64m1, SHL64mCL,
       SHL8m1, SHL8mCL, SHR16m1, SHR16mCL, SHR32m1, SHR32mCL, SHR64m1,
       SHR64mCL, SHR8m1, SHR8mCL, TAILJMPm, TAILJMPm64, TAILJMPm64_REX]
    = []
alignedPairs i ([_, _, _, _, _, _], [])
  | i `elem`
      [RCL16mi, RCL32mi, RCL64mi, RCL8mi, RCR16mi, RCR32mi, RCR64mi,
       RCR8mi]
    = []
alignedPairs i ([_, _, _, _, _, _], [])
  | i `elem` [TCRETURNmi, TCRETURNmi64] = []
alignedPairs i ([_, _, _, _, _, _], [])
  | i `elem`
      [ADC16mi, ADC16mi8, ADC16mr, ADC32mi, ADC32mi8, ADC32mr, ADC64mi32,
       ADC64mi8, ADC64mr, ADC8mi, ADC8mi8, ADC8mr, ADD16mi, ADD16mi8,
       ADD16mr, ADD32mi, ADD32mi8, ADD32mr, ADD64mi32, ADD64mi8, ADD64mr,
       ADD8mi, ADD8mi8, ADD8mr, AND16mi, AND16mi8, AND16mr, AND32mi,
       AND32mi8, AND32mr, AND64mi32, AND64mi8, AND64mr, AND8mi, AND8mi8,
       AND8mr, CMP16mi, CMP16mi8, CMP16mr, CMP32mi, CMP32mi8, CMP32mr,
       CMP64mi32, CMP64mi8, CMP64mr, CMP8mi, CMP8mi8, CMP8mr, CMPXCHG16rm,
       CMPXCHG32rm, CMPXCHG64rm, CMPXCHG8rm, MOV16mi, MOV16mr, MOV32mi,
       MOV32mr, MOV64mi32, MOV64mr, MOV8mi, MOV8mr, MOV8mr_NOREX,
       MOVBE16mr, MOVBE32mr, MOVBE64mr, OR16mi, OR16mi8, OR16mr, OR32mi,
       OR32mi8, OR32mr, OR64mi32, OR64mi8, OR64mr, OR8mi, OR8mi8, OR8mr,
       ROR16mi, ROR32mi, ROR64mi, ROR8mi, SAR16mi, SAR32mi, SAR64mi,
       SAR8mi, SBB16mi, SBB16mi8, SBB16mr, SBB32mi, SBB32mi8, SBB32mr,
       SBB64mi32, SBB64mi8, SBB64mr, SBB8mi, SBB8mi8, SBB8mr, SHL16mi,
       SHL32mi, SHL64mi, SHL8mi, SHR16mi, SHR32mi, SHR64mi, SHR8mi,
       SUB16mi, SUB16mi8, SUB16mr, SUB32mi, SUB32mi8, SUB32mr, SUB64mi32,
       SUB64mi8, SUB64mr, SUB8mi, SUB8mi8, SUB8mr, TEST16mi, TEST32mi,
       TEST64mi32, TEST8mi, XOR16mi, XOR16mi8, XOR16mr, XOR32mi, XOR32mi8,
       XOR32mr, XOR64mi32, XOR64mi8, XOR64mr, XOR8mi, XOR8mi8, XOR8mr]
    = []
alignedPairs i ([_, _, _, _, _, _], [])
  | i `elem` [ROL16mi, ROL32mi, ROL64mi, ROL8mi] = []
alignedPairs i ([_, _, _, _, _, _], [])
  | i `elem`
      [SHLD16mrCL, SHLD32mrCL, SHLD64mrCL, SHRD16mrCL, SHRD32mrCL,
       SHRD64mrCL]
    = []
alignedPairs i ([_, _, _, _, _, _, _], [])
  | i `elem`
      [SHLD16mri8, SHLD32mri8, SHLD64mri8, SHRD16mri8, SHRD32mri8,
       SHRD64mri8]
    = []
alignedPairs i ([_, _, _, _, _, _], [])
  | i `elem` [OR32mrLocked] = []
alignedPairs i ([_], [])
  | i `elem`
      [PUSH16i8, PUSH32i8, PUSH64i32, PUSH64i8, PUSHi16, PUSHi32]
    = []
alignedPairs i ([_, _], []) | i `elem` [ENTER] = []
alignedPairs i ([_], [])
  | i `elem`
      [PUSH16r, PUSH16rmr, PUSH32r, PUSH32rmr, PUSH64r, PUSH64rmr]
    = []
alignedPairs i ([_], [])
  | i `elem`
      [ADC16i16, ADC32i32, ADC64i32, ADC8i8, ADD16i16, ADD32i32,
       ADD64i32, ADD8i8, AND16i16, AND32i32, AND64i32, AND8i8, CMP16i16,
       CMP32i32, CMP64i32, CMP8i8, DIV16r, DIV32r, DIV64r, DIV8r, IDIV16r,
       IDIV32r, IDIV64r, IDIV8r, IMUL16r, IMUL32r, IMUL64r, IMUL8r,
       MUL16r, MUL32r, MUL64r, MUL8r, OR16i16, OR32i32, OR64i32, OR8i8,
       SBB16i16, SBB32i32, SBB64i32, SBB8i8, SUB16i16, SUB32i32, SUB64i32,
       SUB8i8, TEST16i16, TEST32i32, TEST64i32, TEST8i8, XCHG16ar,
       XCHG32ar, XCHG32ar64, XCHG64ar, XOR16i16, XOR32i32, XOR64i32,
       XOR8i8]
    = []
alignedPairs i ([_], [_])
  | i `elem`
      [BSF16rr, BSF32rr, BSF64rr, BSR16rr, BSR32rr, BSR64rr, CMPXCHG16rr,
       CMPXCHG32rr, CMPXCHG64rr, CMPXCHG8rr, MOV16ri, MOV16ri_alt,
       MOV16ri_alt_demat, MOV16ri_alt_remat, MOV16ri_alt_source,
       MOV16ri_demat, MOV16ri_remat, MOV16ri_source, MOV16rr, MOV16rr_REV,
       MOV32r0_demat, MOV32r0_remat, MOV32r1_demat, MOV32r1_remat,
       MOV32r_1_demat, MOV32r_1_remat, MOV32ri, MOV32ri64,
       MOV32ri64_demat, MOV32ri64_remat, MOV32ri64_source, MOV32ri_alt,
       MOV32ri_alt_demat, MOV32ri_alt_remat, MOV32ri_alt_source,
       MOV32ri_demat, MOV32ri_remat, MOV32ri_source, MOV32rr, MOV32rr_REV,
       MOV64ri, MOV64ri32, MOV64ri32_demat, MOV64ri32_remat,
       MOV64ri32_source, MOV64ri_demat, MOV64ri_remat, MOV64ri_source,
       MOV64rr, MOV64rr_REV, MOV8ri, MOV8ri_alt, MOV8ri_demat,
       MOV8ri_remat, MOV8ri_source, MOV8rr, MOV8rr_NOREX, MOV8rr_REV,
       MOVE32, MOVE64, MOVSX16rr8, MOVSX32_NOREXrr8, MOVSX32rr16,
       MOVSX32rr8, MOVSX64rr16, MOVSX64rr32, MOVSX64rr8, MOVZX16rr8,
       MOVZX32_NOREXrr8, MOVZX32rr16, MOVZX32rr8, MOVZX64rr16, MOVZX64rr8,
       POPCNT16rr, POPCNT32rr, POPCNT64rr]
    = []
alignedPairs i ([src], [src'])
  | i `elem` [BSWAP32r, BSWAP64r] = [(src, src')]
alignedPairs i ([_, _], []) | i `elem` [TEST8ri_NOREX] = []
alignedPairs i ([src0, _], [src0'])
  | i `elem` [ADCX32rr, ADCX64rr] = [(src0, src0')]
alignedPairs i ([src0, _, _, _, _, _], [src0'])
  | i `elem` [ADCX32rm, ADCX64rm] = [(src0, src0')]
alignedPairs i ([src1], [src1'])
  | i `elem`
      [DEC16r, DEC16r_alt, DEC32r, DEC32r_alt, DEC64r, DEC8r, INC16r,
       INC16r_alt, INC32r, INC32r_alt, INC64r, INC8r, NEG16r, NEG32r,
       NEG64r, NEG8r, NOT16r, NOT32r, NOT64r, NOT8r, RCL16r1, RCL16rCL,
       RCL32r1, RCL32rCL, RCL64r1, RCL64rCL, RCL8r1, RCL8rCL, RCR16r1,
       RCR16rCL, RCR32r1, RCR32rCL, RCR64r1, RCR64rCL, RCR8r1, RCR8rCL,
       ROL16r1, ROL16rCL, ROL32r1, ROL32rCL, ROL64r1, ROL64rCL, ROL8r1,
       ROL8rCL, ROR16r1, ROR16rCL, ROR32r1, ROR32rCL, ROR64r1, ROR64rCL,
       ROR8r1, ROR8rCL, SAR16r1, SAR16rCL, SAR32r1, SAR32rCL, SAR64r1,
       SAR64rCL, SAR8r1, SAR8rCL, SHL16r1, SHL16rCL, SHL32r1, SHL32rCL,
       SHL64r1, SHL64rCL, SHL8r1, SHL8rCL, SHR16r1, SHR16rCL, SHR32r1,
       SHR32rCL, SHR64r1, SHR64rCL, SHR8r1, SHR8rCL]
    = [(src1, src1')]
alignedPairs i ([src1, _], [src1'])
  | i `elem`
      [RCL16ri, RCL32ri, RCL64ri, RCL8ri, RCR16ri, RCR32ri, RCR64ri,
       RCR8ri]
    = [(src1, src1')]
alignedPairs i ([_, _], [])
  | i `elem`
      [BT16ri8, BT16rr, BT32ri8, BT32rr, BT64ri8, BT64rr, BTC16ri8,
       BTC16rr, BTC32ri8, BTC32rr, BTC64ri8, BTC64rr, BTR16ri8, BTR16rr,
       BTR32ri8, BTR32rr, BTR64ri8, BTR64rr, BTS16ri8, BTS16rr, BTS32ri8,
       BTS32rr, BTS64ri8, BTS64rr, CMP16ri, CMP16ri8, CMP16rr,
       CMP16rr_REV, CMP32ri, CMP32ri8, CMP32rr, CMP32rr_REV, CMP64ri32,
       CMP64ri8, CMP64rr, CMP64rr_REV, CMP8ri, CMP8ri8, CMP8rr,
       CMP8rr_REV, LODSB, LODSL, LODSQ, LODSW, MOV16ao16, MOV16ao32,
       MOV16ao64, MOV32ao16, MOV32ao32, MOV32ao64, MOV64ao32, MOV64ao64,
       MOV8ao16, MOV8ao32, MOV8ao64, TEST16ri, TEST16rr, TEST32ri,
       TEST32rr, TEST64ri32, TEST64rr, TEST8ri, TEST8rr]
    = []
alignedPairs i ([_, _], [_])
  | i `elem`
      [ANDN32rr, ANDN64rr, IMUL16rri, IMUL16rri8, IMUL32rri, IMUL32rri8,
       IMUL64rri32, IMUL64rri8, MOVSB, MOVSL, MOVSQ, MOVSW, RORX32ri,
       RORX64ri, SARX32rr, SARX64rr, SHLX32rr, SHLX64rr, SHRX32rr,
       SHRX64rr]
    = []
alignedPairs i ([src1, _], [src1'])
  | i `elem`
      [ADC16ri, ADC16ri8, ADC16rr, ADC16rr_REV, ADC32ri, ADC32ri8,
       ADC32rr, ADC32rr_REV, ADC64ri32, ADC64ri8, ADC64rr, ADC64rr_REV,
       ADC8ri, ADC8ri8, ADC8rr, ADC8rr_REV, ADD16ri, ADD16ri8, ADD16rr,
       ADD16rr_REV, ADD32ri, ADD32ri8, ADD32rr, ADD32rr_REV, ADD64ri32,
       ADD64ri8, ADD64rr, ADD64rr_REV, ADD8ri, ADD8ri8, ADD8rr,
       ADD8rr_REV, AND16ri, AND16ri8, AND16rr, AND16rr_REV, AND32ri,
       AND32ri8, AND32rr, AND32rr_REV, AND64ri32, AND64ri8, AND64rr,
       AND64rr_REV, AND8ri, AND8ri8, AND8rr, AND8rr_REV, CMOVA16rr,
       CMOVA32rr, CMOVA64rr, CMOVAE16rr, CMOVAE32rr, CMOVAE64rr,
       CMOVB16rr, CMOVB32rr, CMOVB64rr, CMOVBE16rr, CMOVBE32rr,
       CMOVBE64rr, CMOVE16rr, CMOVE32rr, CMOVE64rr, CMOVG16rr, CMOVG32rr,
       CMOVG64rr, CMOVGE16rr, CMOVGE32rr, CMOVGE64rr, CMOVL16rr,
       CMOVL32rr, CMOVL64rr, CMOVLE16rr, CMOVLE32rr, CMOVLE64rr,
       CMOVNE16rr, CMOVNE32rr, CMOVNE64rr, CMOVNO16rr, CMOVNO32rr,
       CMOVNO64rr, CMOVNP16rr, CMOVNP32rr, CMOVNP64rr, CMOVNS16rr,
       CMOVNS32rr, CMOVNS64rr, CMOVO16rr, CMOVO32rr, CMOVO64rr, CMOVP16rr,
       CMOVP32rr, CMOVP64rr, CMOVS16rr, CMOVS32rr, CMOVS64rr, IMUL16rr,
       IMUL32rr, IMUL64rr, OR16ri, OR16ri8, OR16rr, OR16rr_REV, OR32ri,
       OR32ri8, OR32rr, OR32rr_REV, OR64ri32, OR64ri8, OR64rr, OR64rr_REV,
       OR8ri, OR8ri8, OR8rr, OR8rr_REV, ROL16ri, ROL32ri, ROL64ri, ROL8ri,
       ROR16ri, ROR32ri, ROR64ri, ROR8ri, SAR16ri, SAR32ri, SAR64ri,
       SAR8ri, SBB16ri, SBB16ri8, SBB16rr, SBB16rr_REV, SBB32ri, SBB32ri8,
       SBB32rr, SBB32rr_REV, SBB64ri32, SBB64ri8, SBB64rr, SBB64rr_REV,
       SBB8ri, SBB8ri8, SBB8rr, SBB8rr_REV, SHL16ri, SHL32ri, SHL64ri,
       SHL8ri, SHLD16rrCL, SHLD32rrCL, SHLD64rrCL, SHR16ri, SHR32ri,
       SHR64ri, SHR8ri, SHRD16rrCL, SHRD32rrCL, SHRD64rrCL, SUB16ri,
       SUB16ri8, SUB16rr, SUB16rr_REV, SUB32ri, SUB32ri8, SUB32rr,
       SUB32rr_REV, SUB64ri32, SUB64ri8, SUB64rr, SUB64rr_REV, SUB8ri,
       SUB8ri8, SUB8rr, SUB8rr_REV, XOR16ri, XOR16ri8, XOR16rr,
       XOR16rr_REV, XOR32ri, XOR32ri8, XOR32rr, XOR32rr_REV, XOR64ri32,
       XOR64ri8, XOR64rr, XOR64rr_REV, XOR8ri, XOR8ri8, XOR8rr,
       XOR8rr_REV]
    = [(src1, src1')]
alignedPairs i ([src1, _, _], [src1'])
  | i `elem`
      [SHLD16rri8, SHLD32rri8, SHLD64rri8, SHRD16rri8, SHRD32rri8,
       SHRD64rri8]
    = [(src1, src1')]
alignedPairs i ([_, _, _, _, _], [])
  | i `elem`
      [DIV16m, DIV32m, DIV64m, DIV8m, IDIV16m, IDIV32m, IDIV64m, IDIV8m,
       IMUL16m, IMUL32m, IMUL64m, IMUL8m, MUL16m, MUL32m, MUL64m, MUL8m,
       PUSH16rmm, PUSH32rmm, PUSH64rmm]
    = []
alignedPairs i ([_, _, _, _, _], [_])
  | i `elem`
      [BSF16rm, BSF32rm, BSF64rm, BSR16rm, BSR32rm, BSR64rm, LEA16r,
       LEA32r, LEA64_32r, LEA64r, MOV16rm, MOV32rm, MOV64rm, MOV8rm,
       MOV8rm_NOREX, MOVBE16rm, MOVBE32rm, MOVBE64rm, MOVSX16rm8,
       MOVSX32_NOREXrm8, MOVSX32rm16, MOVSX32rm8, MOVSX64rm16,
       MOVSX64rm32, MOVSX64rm8, MOVZX16rm8, MOVZX32_NOREXrm8, MOVZX32rm16,
       MOVZX32rm8, MOVZX64rm16, MOVZX64rm8, POPCNT16rm, POPCNT32rm,
       POPCNT64rm]
    = []
alignedPairs i ([_, _, _, _, _, _], [])
  | i `elem`
      [CMP16rm, CMP32rm, CMP64rm, CMP8rm, TEST16rm, TEST32rm, TEST64rm,
       TEST8rm]
    = []
alignedPairs i ([_, _, _, _, _, _], [_])
  | i `elem` [ANDN32rm, ANDN64rm] = []
alignedPairs i ([src1, _, _, _, _, _], [src1'])
  | i `elem`
      [ADC16rm, ADC32rm, ADC64rm, ADC8rm, ADD16rm, ADD32rm, ADD64rm,
       ADD8rm, AND16rm, AND32rm, AND64rm, AND8rm, CMOVA16rm, CMOVA32rm,
       CMOVA64rm, CMOVAE16rm, CMOVAE32rm, CMOVAE64rm, CMOVB16rm,
       CMOVB32rm, CMOVB64rm, CMOVBE16rm, CMOVBE32rm, CMOVBE64rm,
       CMOVE16rm, CMOVE32rm, CMOVE64rm, CMOVG16rm, CMOVG32rm, CMOVG64rm,
       CMOVGE16rm, CMOVGE32rm, CMOVGE64rm, CMOVL16rm, CMOVL32rm,
       CMOVL64rm, CMOVLE16rm, CMOVLE32rm, CMOVLE64rm, CMOVNE16rm,
       CMOVNE32rm, CMOVNE64rm, CMOVNO16rm, CMOVNO32rm, CMOVNO64rm,
       CMOVNP16rm, CMOVNP32rm, CMOVNP64rm, CMOVNS16rm, CMOVNS32rm,
       CMOVNS64rm, CMOVO16rm, CMOVO32rm, CMOVO64rm, CMOVP16rm, CMOVP32rm,
       CMOVP64rm, CMOVS16rm, CMOVS32rm, CMOVS64rm, IMUL16rm, IMUL32rm,
       IMUL64rm, OR16rm, OR32rm, OR64rm, OR8rm, SBB16rm, SBB32rm, SBB64rm,
       SBB8rm, SUB16rm, SUB32rm, SUB64rm, SUB8rm, XOR16rm, XOR32rm,
       XOR64rm, XOR8rm]
    = [(src1, src1')]
alignedPairs i ([_, _, _, _, _, _], [])
  | i `elem`
      [BT16mi8, BT16mr, BT32mi8, BT32mr, BT64mi8, BT64mr, BTC16mi8,
       BTC16mr, BTC32mi8, BTC32mr, BTC64mi8, BTC64mr, BTR16mi8, BTR16mr,
       BTR32mi8, BTR32mr, BTR64mi8, BTR64mr, BTS16mi8, BTS16mr, BTS32mi8,
       BTS32mr, BTS64mi8, BTS64mr]
    = []
alignedPairs i ([_, _, _, _, _, _], [_])
  | i `elem`
      [IMUL16rmi, IMUL16rmi8, IMUL32rmi, IMUL32rmi8, IMUL64rmi32,
       IMUL64rmi8, RORX32mi, RORX64mi, SARX32rm, SARX64rm, SHLX32rm,
       SHLX64rm, SHRX32rm, SHRX64rm]
    = []
alignedPairs i ([_], []) | i `elem` [RETL, RETQ] = []
alignedPairs i ([val, _, _, _, _, _], [val'])
  | i `elem` [XCHG16rm, XCHG32rm, XCHG64rm, XCHG8rm] = [(val, val')]
alignedPairs i ([val, _], [val'])
  | i `elem` [XCHG16rr, XCHG32rr, XCHG64rr, XCHG8rr] = [(val, val')]
alignedPairs i ([_, _, _, _, _], []) | i `elem` [NOOPL, NOOPW] = []
alignedPairs _ _ = []

