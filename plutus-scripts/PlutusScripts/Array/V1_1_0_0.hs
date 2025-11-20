{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Array.V1_1_0_0 where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Array.Common (
  mkIndexArrayPolicy,
  mkLengthOfArrayPolicy,
  mkListToArrayPolicy,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

succeedingIndexArrayPolicyCompiledV1_1_0_0
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingIndexArrayPolicyCompiledV1_1_0_0 =
  $$(PlutusTx.compile [||mkIndexArrayPolicy||])

succeedingIndexArrayPolicyScriptV1_1_0_0 :: SerialisedScript
succeedingIndexArrayPolicyScriptV1_1_0_0 =
  serialiseCompiledCode succeedingIndexArrayPolicyCompiledV1_1_0_0

succeedingLengthOfArrayPolicyCompiledV1_1_0_0
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingLengthOfArrayPolicyCompiledV1_1_0_0 =
  $$(PlutusTx.compile [||mkLengthOfArrayPolicy||])

succeedingLengthOfArrayPolicyScriptV1_1_0_0 :: SerialisedScript
succeedingLengthOfArrayPolicyScriptV1_1_0_0 =
  serialiseCompiledCode succeedingLengthOfArrayPolicyCompiledV1_1_0_0

succeedingListToArrayPolicyCompiledV1_1_0_0
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingListToArrayPolicyCompiledV1_1_0_0 =
  $$(PlutusTx.compile [||mkListToArrayPolicy||])

succeedingListToArrayPolicyScriptV1_1_0_0 :: SerialisedScript
succeedingListToArrayPolicyScriptV1_1_0_0 =
  serialiseCompiledCode succeedingListToArrayPolicyCompiledV1_1_0_0
