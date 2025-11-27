{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Batch6.ExpModInteger.V3_100 (
  failingExpModIntegerScriptGroup,
  succeedingExpModIntegerPolicy,
  succeedingExpModIntegerInversePolicy,
  succeedingExponentOnePolicy,
)
where

import Helpers.ScriptUtils (ScriptGroup (ScriptGroup, sgBaseName, sgScripts))
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion100)
import PlutusScripts.Batch6.ExpModInteger.Common (
  failingExpModIntegerParams,
  mkExpModIntegerInversePolicy,
  mkExponentOnePolicy,
  mkSimpleExpModIntegerPolicy,
  succeedingExponentOneParams,
  succeedingInverseParams,
  succeedingSimpleExpModIntegerParams,
 )
import PlutusTx (compile, liftCode, unsafeApplyCode)
import PlutusTx.Code (CompiledCodeIn)
import PlutusTx.Prelude qualified as P

-- Compiled code values with parameters already applied for succeeding tests
succeedingExpModIntegerPolicy :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingExpModIntegerPolicy =
  $$(compile [||mkSimpleExpModIntegerPolicy||])
    `unsafeApplyCode` liftCode plcVersion100 succeedingSimpleExpModIntegerParams

succeedingExpModIntegerInversePolicy :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingExpModIntegerInversePolicy =
  $$(compile [||mkExpModIntegerInversePolicy||])
    `unsafeApplyCode` liftCode plcVersion100 succeedingInverseParams

succeedingExponentOnePolicy
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingExponentOnePolicy =
  $$(compile [||mkExponentOnePolicy||])
    `unsafeApplyCode` liftCode plcVersion100 succeedingExponentOneParams

failingExpModIntegerScriptGroup
  :: ScriptGroup DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
failingExpModIntegerScriptGroup =
  ScriptGroup
    { sgBaseName = "failingExpModIntegerPolicyScript_V3_100"
    , sgScripts = map compileScript failingExpModIntegerParams
    }
 where
  compileScript param =
    $$(compile [||mkSimpleExpModIntegerPolicy||])
      `unsafeApplyCode` liftCode plcVersion100 [param]
