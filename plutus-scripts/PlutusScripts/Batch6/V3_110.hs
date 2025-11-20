{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Batch6.V3_110 where

import Helpers.ScriptUtils (ScriptGroup (ScriptGroup, sgBaseName, sgScripts))
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion110)
import PlutusScripts.Batch6.DropList qualified as DropList
import PlutusTx (compile, liftCode, unsafeApplyCode)
import PlutusTx.Code (CompiledCodeIn)
import PlutusTx.Prelude qualified as P

-- Compiled code values with parameters already applied for succeeding tests
succeedingDropListPolicyCompiled_V3_110
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingDropListPolicyCompiled_V3_110 =
  $$(compile [||DropList.mkDropListPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 DropList.succeedingDropListParams

-- These should fail due to exceeding the budget.
expensiveDropListScriptGroup_V3_110 :: ScriptGroup DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
expensiveDropListScriptGroup_V3_110 =
  ScriptGroup
    { sgBaseName = "expensiveDropListPolicyScript_V3_110"
    , sgScripts = map compileDropList DropList.expensiveDropListParams
    }
 where
  compileDropList param =
    $$(compile [||DropList.mkDropListPolicy||])
      `unsafeApplyCode` liftCode plcVersion110 [param]
