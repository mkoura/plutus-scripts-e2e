{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Batch6.Array.V2_110 where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Batch6.Array.Common (
  mkIndexArrayPolicy,
  mkLengthOfArrayPolicy,
  mkListToArrayPolicy,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

succeedingIndexArrayPolicyCompiled_V2_110
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingIndexArrayPolicyCompiled_V2_110 =
  $$(PlutusTx.compile [||mkIndexArrayPolicy||])

succeedingIndexArrayPolicyScript_V2_110 :: SerialisedScript
succeedingIndexArrayPolicyScript_V2_110 =
  serialiseCompiledCode succeedingIndexArrayPolicyCompiled_V2_110

succeedingLengthOfArrayPolicyCompiled_V2_110
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingLengthOfArrayPolicyCompiled_V2_110 =
  $$(PlutusTx.compile [||mkLengthOfArrayPolicy||])

succeedingLengthOfArrayPolicyScript_V2_110 :: SerialisedScript
succeedingLengthOfArrayPolicyScript_V2_110 =
  serialiseCompiledCode succeedingLengthOfArrayPolicyCompiled_V2_110

succeedingListToArrayPolicyCompiled_V2_110
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingListToArrayPolicyCompiled_V2_110 =
  $$(PlutusTx.compile [||mkListToArrayPolicy||])

succeedingListToArrayPolicyScript_V2_110 :: SerialisedScript
succeedingListToArrayPolicyScript_V2_110 =
  serialiseCompiledCode succeedingListToArrayPolicyCompiled_V2_110
