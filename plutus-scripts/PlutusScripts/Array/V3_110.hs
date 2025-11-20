{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Array.V3_110 where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Array.Common (
  mkIndexArrayPolicy,
  mkLengthOfArrayPolicy,
  mkListToArrayPolicy,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

succeedingIndexArrayPolicyCompiled_V3_110
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingIndexArrayPolicyCompiled_V3_110 =
  $$(PlutusTx.compile [||mkIndexArrayPolicy||])

succeedingIndexArrayPolicyScript_V3_110 :: SerialisedScript
succeedingIndexArrayPolicyScript_V3_110 =
  serialiseCompiledCode succeedingIndexArrayPolicyCompiled_V3_110

succeedingLengthOfArrayPolicyCompiled_V3_110
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingLengthOfArrayPolicyCompiled_V3_110 =
  $$(PlutusTx.compile [||mkLengthOfArrayPolicy||])

succeedingLengthOfArrayPolicyScript_V3_110 :: SerialisedScript
succeedingLengthOfArrayPolicyScript_V3_110 =
  serialiseCompiledCode succeedingLengthOfArrayPolicyCompiled_V3_110

succeedingListToArrayPolicyCompiled_V3_110
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingListToArrayPolicyCompiled_V3_110 =
  $$(PlutusTx.compile [||mkListToArrayPolicy||])

succeedingListToArrayPolicyScript_V3_110 :: SerialisedScript
succeedingListToArrayPolicyScript_V3_110 =
  serialiseCompiledCode succeedingListToArrayPolicyCompiled_V3_110
