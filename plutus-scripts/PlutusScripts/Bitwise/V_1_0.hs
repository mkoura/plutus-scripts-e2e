{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Bitwise.V_1_0 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusCore.Version (plcVersion100)
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Bitwise.Conversions (
--  ByteStringToIntegerParams,
--  IntegerToByteStringParams,
  mkByteStringToIntegerRoundtripPolicySimple
  )
import PlutusScripts.Bitwise.Complement (
  mkComplementByteStringPolicy,
  succeedingComplementByteStringParams
  )
import PlutusScripts.Bitwise.Logical (
  mkAndByteStringPolicy,
  mkOrByteStringPolicy,
  mkXorByteStringPolicy,
  succeedingAndByteStringParams,
  succeedingOrByteStringParams,
  succeedingXorByteStringParams
  )
import PlutusScripts.Helpers (writeSerialisedScript)
import PlutusTx qualified

-- integerToByteString and byteStringToInteger added to PlutusV2 --

byteStringToIntegerRoundtripPolicyV2 :: SerialisedScript
byteStringToIntegerRoundtripPolicyV2 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||mkByteStringToIntegerRoundtripPolicySimple||])

integerToByteStringPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
integerToByteStringPolicyScriptV2 = C.PlutusScriptSerialised byteStringToIntegerRoundtripPolicyV2

writeIntegerToByteStringPolicyScriptV2 :: IO ()
writeIntegerToByteStringPolicyScriptV2 = writeSerialisedScript "byteStringToIntegerRoundtripPolicyV2" integerToByteStringPolicyScriptV2

-- Simple end-to-end tests for bitwise builtins in PlutusV3.  All of these are
-- self-contained: the inputs are compiled into the script rather than being
-- obtained from a redeemer.

writeV3Script
  :: PlutusTx.Lift DefaultUni params
  => String
  -> (PlutusTx.CompiledCodeIn  DefaultUni DefaultFun (params -> r))
  -> params
  -> IO ()
writeV3Script name code params =
  let script :: C.PlutusScript C.PlutusScriptV3
      script = C.PlutusScriptSerialised $ serialiseCompiledCode (code `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion100 params))
  in writeSerialisedScript name script

writeAndByteStringPolicyScriptsV3 :: IO ()
writeAndByteStringPolicyScriptsV3 =
  writeV3Script
  "andByteStringPolicyScriptV3"
  $$(PlutusTx.compile [|| mkAndByteStringPolicy ||])
  succeedingAndByteStringParams

writeOrByteStringPolicyScriptsV3 :: IO ()
writeOrByteStringPolicyScriptsV3 =
  writeV3Script
  "orByteStringPolicyScriptV3"
  $$(PlutusTx.compile [|| mkOrByteStringPolicy ||])
  succeedingOrByteStringParams

writeXorByteStringPolicyScriptsV3 :: IO ()
writeXorByteStringPolicyScriptsV3 =
  writeV3Script
  "xorByteStringPolicyScriptV3"
  $$(PlutusTx.compile [|| mkXorByteStringPolicy ||])
  succeedingXorByteStringParams

writeComplementByteStringPolicyScriptsV3 :: IO ()
writeComplementByteStringPolicyScriptsV3 =
  writeV3Script
  "complementByteStringPolicyScriptV3"
  $$(PlutusTx.compile [|| mkComplementByteStringPolicy ||])
  succeedingComplementByteStringParams
