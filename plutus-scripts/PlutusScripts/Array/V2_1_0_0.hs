{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Array.V2_1_0_0 where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Array.Common (
  mkIndexArrayPolicy,
  mkLengthOfArrayPolicy,
  mkListToArrayPolicy,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

succeedingIndexArrayPolicyCompiledV2_1_0_0
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingIndexArrayPolicyCompiledV2_1_0_0 =
  $$(PlutusTx.compile [||mkIndexArrayPolicy||])

succeedingIndexArrayPolicyScriptV2_1_0_0 :: SerialisedScript
succeedingIndexArrayPolicyScriptV2_1_0_0 =
  serialiseCompiledCode succeedingIndexArrayPolicyCompiledV2_1_0_0

succeedingLengthOfArrayPolicyCompiledV2_1_0_0
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingLengthOfArrayPolicyCompiledV2_1_0_0 =
  $$(PlutusTx.compile [||mkLengthOfArrayPolicy||])

succeedingLengthOfArrayPolicyScriptV2_1_0_0 :: SerialisedScript
succeedingLengthOfArrayPolicyScriptV2_1_0_0 =
  serialiseCompiledCode succeedingLengthOfArrayPolicyCompiledV2_1_0_0

succeedingListToArrayPolicyCompiledV2_1_0_0
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingListToArrayPolicyCompiledV2_1_0_0 =
  $$(PlutusTx.compile [||mkListToArrayPolicy||])

succeedingListToArrayPolicyScriptV2_1_0_0 :: SerialisedScript
succeedingListToArrayPolicyScriptV2_1_0_0 =
  serialiseCompiledCode succeedingListToArrayPolicyCompiledV2_1_0_0
