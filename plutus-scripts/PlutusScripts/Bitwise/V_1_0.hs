{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Bitwise.V_1_0 where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Bitwise.Conversions (
  mkByteStringToIntegerRoundtripPolicySimple,
 )
import PlutusTx qualified

-- integerToByteString and byteStringToInteger added to PlutusV2 --

byteStringToIntegerRoundtripPolicyCompiledV2
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
byteStringToIntegerRoundtripPolicyCompiledV2 =
  $$(PlutusTx.compile [||mkByteStringToIntegerRoundtripPolicySimple||])

byteStringToIntegerRoundtripPolicyV2 :: SerialisedScript
byteStringToIntegerRoundtripPolicyV2 =
  serialiseCompiledCode byteStringToIntegerRoundtripPolicyCompiledV2
