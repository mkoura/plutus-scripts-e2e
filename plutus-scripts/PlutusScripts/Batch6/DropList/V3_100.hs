{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Batch6.DropList.V3_100 where

import Helpers.ScriptUtils (ScriptGroup (ScriptGroup, sgBaseName, sgScripts))
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion100)
import PlutusScripts.Batch6.DropList.Common qualified as DropList
import PlutusTx (compile, liftCode, unsafeApplyCode)
import PlutusTx.Code (CompiledCodeIn)
import PlutusTx.Prelude qualified as P

-- Tests for `dropList` with PlutusV2 and Plutus 1.0.0

-- FIXME: these are currently unused until we work out how to deal with the fact
-- that SoPs and all builtins will be enabled in PlutusV1 and PlutusV2 at PV11.

-- Compiled code values with parameters already applied for succeeding tests
succeedingDropListPolicy
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingDropListPolicy =
  $$(compile [||DropList.mkDropListPolicy||])
    `unsafeApplyCode` liftCode plcVersion100 DropList.succeedingDropListParams

-- These should fail due to exceeding the budget.
expensiveDropListScriptGroup :: ScriptGroup DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
expensiveDropListScriptGroup =
  ScriptGroup
    { sgBaseName = "expensiveDropListPolicyScript_V2_100"
    , sgScripts = map compileDropList DropList.expensiveDropListParams
    }
 where
  compileDropList param =
    $$(compile [||DropList.mkDropListPolicy||])
      `unsafeApplyCode` liftCode plcVersion100 [param]
