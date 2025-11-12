{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Bitwise.V_1_0 where

import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV2, PlutusV3))
import PlutusScripts.Bitwise.Conversions (
  --  ByteStringToIntegerParams,
  --  IntegerToByteStringParams,
  mkByteStringToIntegerRoundtripPolicySimple,
 )
import PlutusScripts.Bitwise.Logical (
  mkAndByteStringPolicy,
  mkOrByteStringPolicy,
  mkXorByteStringPolicy,
  succeedingAndByteStringParams,
  succeedingOrByteStringParams,
  succeedingXorByteStringParams,
 )
import PlutusScripts.Helpers (writeCompiledScript)
import PlutusTx qualified

-- integerToByteString and byteStringToInteger added to PlutusV2 --

byteStringToIntegerRoundtripPolicyCompiledV2 ::
  PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
byteStringToIntegerRoundtripPolicyCompiledV2 =
  $$(PlutusTx.compile [||mkByteStringToIntegerRoundtripPolicySimple||])

byteStringToIntegerRoundtripPolicyV2 :: SerialisedScript
byteStringToIntegerRoundtripPolicyV2 =
  serialiseCompiledCode byteStringToIntegerRoundtripPolicyCompiledV2

writeIntegerToByteStringPolicyScriptV2 :: IO ()
writeIntegerToByteStringPolicyScriptV2 =
  writeCompiledScript
    PlutusV2
    "byteStringToIntegerRoundtripPolicyV2"
    byteStringToIntegerRoundtripPolicyCompiledV2

-- Simple end-to-end tests for bitwise builtins in PlutusV3.  All of these are
-- self-contained: the inputs are compiled into the script rather than being
-- obtained from a redeemer.

-- These are not currently used (see Spec.WriteScriptFiles), but they
-- demonstrate that the code does work.  Note that the script names are the same
-- as those of the V_1_1 scripts: if we ever need these then they'll have to be
-- either renamed or written into a different directory.
writeV3Script
  :: PlutusTx.Lift DefaultUni params
  => String
  -> (PlutusTx.CompiledCodeIn DefaultUni DefaultFun (params -> r))
  -> params
  -> IO ()
writeV3Script name code params =
  let compiledCode = code `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion100 params)
   in writeCompiledScript PlutusV3 name compiledCode

writeAndByteStringPolicyScriptsV3 :: IO ()
writeAndByteStringPolicyScriptsV3 =
  writeV3Script
    "andByteStringPolicyScriptV3"
    $$(PlutusTx.compile [||mkAndByteStringPolicy||])
    succeedingAndByteStringParams

writeOrByteStringPolicyScriptsV3 :: IO ()
writeOrByteStringPolicyScriptsV3 =
  writeV3Script
    "orByteStringPolicyScriptV3"
    $$(PlutusTx.compile [||mkOrByteStringPolicy||])
    succeedingOrByteStringParams

writeXorByteStringPolicyScriptsV3 :: IO ()
writeXorByteStringPolicyScriptsV3 =
  writeV3Script
    "xorByteStringPolicyScriptV3"
    $$(PlutusTx.compile [||mkXorByteStringPolicy||])
    succeedingXorByteStringParams
