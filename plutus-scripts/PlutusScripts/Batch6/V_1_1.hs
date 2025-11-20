{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Batch6.V_1_1 where

import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion110)
import PlutusScripts.Batch6.DropList qualified as DropList
import PlutusTx (compile, liftCode, unsafeApplyCode)
import PlutusTx.Code (CompiledCodeIn)
import PlutusTx.Prelude qualified as P
import Helpers.ScriptUtils (ScriptGroup (ScriptGroup, sgBaseName, sgScripts))

-- Compiled code values with parameters already applied for succeeding tests
succeedingDropListPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingDropListPolicyCompiledV3 =
  $$(compile [|| DropList.mkDropListPolicy ||])
    `unsafeApplyCode` liftCode plcVersion110 DropList.succeedingDropListParams

failingDropListScriptGroupV3 :: ScriptGroup DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
failingDropListScriptGroupV3 =  ScriptGroup
      { sgBaseName = "failingDropListPolicyScriptV3"
      , sgScripts = map compileDropList DropList.failingDropListParams
      }
  where compileDropList param =
          $$(compile [|| DropList.mkDropListPolicy ||])
          `unsafeApplyCode` liftCode plcVersion110 [param]
