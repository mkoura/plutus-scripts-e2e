{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Array.V1_100 where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Array.Common (
  mkIndexArrayPolicy,
  mkLengthOfArrayPolicy,
  mkListToArrayPolicy,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

succeedingIndexArrayPolicyCompiled_V1_100
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingIndexArrayPolicyCompiled_V1_100 =
  $$(PlutusTx.compile [||mkIndexArrayPolicy||])

succeedingIndexArrayPolicyScript_V1_100 :: SerialisedScript
succeedingIndexArrayPolicyScript_V1_100 =
  serialiseCompiledCode succeedingIndexArrayPolicyCompiled_V1_100

succeedingLengthOfArrayPolicyCompiled_V1_100
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingLengthOfArrayPolicyCompiled_V1_100 =
  $$(PlutusTx.compile [||mkLengthOfArrayPolicy||])

succeedingLengthOfArrayPolicyScript_V1_100 :: SerialisedScript
succeedingLengthOfArrayPolicyScript_V1_100 =
  serialiseCompiledCode succeedingLengthOfArrayPolicyCompiled_V1_100

succeedingListToArrayPolicyCompiled_V1_100
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingListToArrayPolicyCompiled_V1_100 =
  $$(PlutusTx.compile [||mkListToArrayPolicy||])

succeedingListToArrayPolicyScript_V1_100 :: SerialisedScript
succeedingListToArrayPolicyScript_V1_100 =
  serialiseCompiledCode succeedingListToArrayPolicyCompiled_V1_100
