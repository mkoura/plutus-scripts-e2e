{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Array.V3_1_1_0 where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Array.Common (
  mkIndexArrayPolicy,
  mkLengthOfArrayPolicy,
  mkListToArrayPolicy,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

succeedingIndexArrayPolicyCompiledV3_1_1_0
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingIndexArrayPolicyCompiledV3_1_1_0 =
  $$(PlutusTx.compile [||mkIndexArrayPolicy||])

succeedingIndexArrayPolicyScriptV3_1_1_0 :: SerialisedScript
succeedingIndexArrayPolicyScriptV3_1_1_0 =
  serialiseCompiledCode succeedingIndexArrayPolicyCompiledV3_1_1_0

succeedingLengthOfArrayPolicyCompiledV3_1_1_0
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingLengthOfArrayPolicyCompiledV3_1_1_0 =
  $$(PlutusTx.compile [||mkLengthOfArrayPolicy||])

succeedingLengthOfArrayPolicyScriptV3_1_1_0 :: SerialisedScript
succeedingLengthOfArrayPolicyScriptV3_1_1_0 =
  serialiseCompiledCode succeedingLengthOfArrayPolicyCompiledV3_1_1_0

succeedingListToArrayPolicyCompiledV3_1_1_0
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingListToArrayPolicyCompiledV3_1_1_0 =
  $$(PlutusTx.compile [||mkListToArrayPolicy||])

succeedingListToArrayPolicyScriptV3_1_1_0 :: SerialisedScript
succeedingListToArrayPolicyScriptV3_1_1_0 =
  serialiseCompiledCode succeedingListToArrayPolicyCompiledV3_1_1_0
