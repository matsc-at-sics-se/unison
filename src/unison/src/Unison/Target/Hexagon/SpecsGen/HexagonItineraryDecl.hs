-- This file has been generated by specsgen. Do not modify by hand!

module Unison.Target.Hexagon.SpecsGen.HexagonItineraryDecl
       (HexagonItinerary(..)) where
 
data HexagonItinerary = S_2op_tc_2_SLOT23
                      | S_2op_tc_1_SLOT23
                      | ALU32_3op_tc_1_SLOT0123
                      | ALU64_tc_1_SLOT23
                      | ALU64_tc_2_SLOT23
                      | ALU32_ADDI_tc_1_SLOT0123
                      | ALU32_3op_tc_2_SLOT0123
                      | ALU32_2op_tc_1_SLOT0123
                      | CR_tc_3x_SLOT3
                      | ALU64_tc_2early_SLOT23
                      | M_tc_3x_SLOT23
                      | S_3op_tc_1_SLOT23
                      | S_3op_tc_2early_SLOT23
                      | S_3op_tc_2_SLOT23
                      | EXTENDER_tc_1_SLOT0123
                      | S_3op_tc_3_SLOT23
                      | M_tc_3stall_SLOT23
                      | PSEUDO
                      | NoItinerary
                      | CR_tc_2early_SLOT23
                      | S_2op_tc_2early_SLOT23
                      | ALU32_3op_tc_2early_SLOT0123
                      | ALU32_2op_tc_2early_SLOT0123
                      | CR_tc_2_SLOT3
                      | J_tc_2early_SLOT2
                      | J_tc_2early_SLOT23
                      | LD_tc_ld_SLOT01
                      | DUPLEX
                      | J_tc_2early_SLOT0123
                      | S_2op_tc_3or4x_SLOT23
                      | ALU64_tc_3x_SLOT23
                      | M_tc_3or4x_SLOT23
                      | M_tc_3_SLOT23
                      | CVI_VA
                      | PSEUDOM
                      | CR_tc_2early_SLOT3
                      | NCJ_tc_3or4stall_SLOT0
                      | COMPOUND
                      | V2LDST_tc_ld_SLOT01
                      | LD_tc_ld_SLOT0
                      | V4LDST_tc_st_SLOT0
                      | V4LDST_tc_ld_SLOT01
                      | LD_tc_3or4stall_SLOT0
                      | CVI_VM_LD
                      | M_tc_2_SLOT23
                      | S_3op_tc_3x_SLOT23
                      | ST_tc_ld_SLOT0
                      | V2LDST_tc_st_SLOT01
                      | ST_tc_st_SLOT01
                      | V2LDST_tc_st_SLOT0
                      | ST_tc_st_SLOT0
                      | V4LDST_tc_st_SLOT01
                      | CVI_VM_ST
                      | PREFIX
                      | CVI_VX_LATE
                      | CVI_VA_DV
                      | CVI_VP_LONG
                      | CVI_VM_VP_LDU
                      | CVI_VM_CUR_LD
                      | CVI_VM_TMP_LD
                      | CVI_VM_STU
                      | CVI_VM_NEW_ST
                      | CVI_VX
                      | CVI_VX_DV
                      | CVI_VS
                      | CVI_VP_VS_LONG_EARLY
                      | CVI_VP
                      | CVI_VP_VS_LONG
                      | CVI_HIST
                      | CVI_VX_DV_LONG
                      | CVI_VX_LONG
                      | CVI_VINLANESAT
                      | CVI_VP_VS
                      | ST_tc_3stall_SLOT0
                      deriving (Eq, Read, Show)

