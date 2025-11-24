{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Batch6.Array.V1_110 where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Batch6.Array.Common (
  mkIndexArrayPolicy,
  mkLengthOfArrayPolicy,
  mkListToArrayPolicy,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

succeedingIndexArrayPolicyCompiled_V1_110
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingIndexArrayPolicyCompiled_V1_110 =
  $$(PlutusTx.compile [||mkIndexArrayPolicy||])

succeedingIndexArrayPolicyScript_V1_110 :: SerialisedScript
succeedingIndexArrayPolicyScript_V1_110 =
  serialiseCompiledCode succeedingIndexArrayPolicyCompiled_V1_110

succeedingLengthOfArrayPolicyCompiled_V1_110
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingLengthOfArrayPolicyCompiled_V1_110 =
  $$(PlutusTx.compile [||mkLengthOfArrayPolicy||])

succeedingLengthOfArrayPolicyScript_V1_110 :: SerialisedScript
succeedingLengthOfArrayPolicyScript_V1_110 =
  serialiseCompiledCode succeedingLengthOfArrayPolicyCompiled_V1_110

succeedingListToArrayPolicyCompiled_V1_110
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingListToArrayPolicyCompiled_V1_110 =
  $$(PlutusTx.compile [||mkListToArrayPolicy||])

succeedingListToArrayPolicyScript_V1_110 :: SerialisedScript
succeedingListToArrayPolicyScript_V1_110 =
  serialiseCompiledCode succeedingListToArrayPolicyCompiled_V1_110
