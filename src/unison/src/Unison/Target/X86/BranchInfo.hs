module Unison.Target.X86.BranchInfo (branchInfo) where

import Unison
import Unison.Target.X86.SpecsGen.X86InstructionDecl
import Unison.Target.X86.SpecsGen()

-- | Gives the target of a jump instruction and the type of jump

branchInfo (Branch {oBranchIs = is, oBranchUs = (BlockRef l:_)})
  | targetInst is `elem` [JAE_1, JAE_2, JAE_4, JA_1, JA_2, JA_4, JBE_1, JBE_2, JBE_4,
     JB_1, JB_2, JB_4, JCXZ, JECXZ, JE_1, JE_2, JE_4, JGE_1, JGE_2,
     JGE_4, JG_1, JG_2, JG_4, JLE_1, JLE_2, JLE_4, JL_1, JL_2, JL_4,
     JNE_1, JNE_2, JNE_4, JNO_1, JNO_2, JNO_4, JNP_1, JNP_2,
     JNP_4, JNS_1, JNS_2, JNS_4, JO_1, JO_2, JO_4, JP_1, JP_2, JP_4,
     JRCXZ, JS_1, JS_2, JS_4, LOOP, LOOPE, LOOPNE] =
    BranchInfo Conditional (Just l)
  | targetInst is `elem` [JMP_1, JMP_2, JMP_4] =
    BranchInfo Unconditional (Just l)
branchInfo (Branch {oBranchIs = is})
  | targetInst is `elem` [JMP16m, JMP16r, JMP32m, JMP32r, JMP64m, JMP64r,
                          RETIL, RETIQ, RETIW, RETL, RETQ, RETW, LEAVE, LEAVE64] =
    BranchInfo Unconditional Nothing

branchInfo o = error ("unmatched pattern: branchInfo " ++ show (mkSingleOperation (-1) (Natural o)))
