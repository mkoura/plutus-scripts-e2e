{-# LANGUAGE DataKinds           #-}
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
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Bitwise.Conversions (
--  ByteStringToIntegerParams,
--  IntegerToByteStringParams,
  mkByteStringToIntegerRoundtripPolicySimple
  )
import PlutusScripts.Bitwise.Complement (
  mkComplementByteStringSucceedingPolicy,
  complementByteStringParams
  )
import PlutusScripts.Bitwise.Logical (
  mkAndByteStringSucceedingPolicy,
  mkOrByteStringSucceedingPolicy,
  mkXorByteStringSucceedingPolicy,
  andByteStringParams,
  orByteStringParams,
  xorByteStringParams
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

andByteStringSucceedingPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
andByteStringSucceedingPolicyScriptV3 =
  let params = PlutusTx.liftCode plcVersion100 andByteStringParams
  in C.PlutusScriptSerialised $
     serialiseCompiledCode $ $$(PlutusTx.compile [|| mkAndByteStringSucceedingPolicy ||])
     `PlutusTx.unsafeApplyCode` params

writeAndByteStringPolicyScriptsV3 :: IO ()
writeAndByteStringPolicyScriptsV3 =
  writeSerialisedScript "andByteStringSucceedingPolicyV3" andByteStringSucceedingPolicyScriptV3

orByteStringSucceedingPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
orByteStringSucceedingPolicyScriptV3 =
  let params = PlutusTx.liftCode plcVersion100 orByteStringParams
  in C.PlutusScriptSerialised $
     serialiseCompiledCode $ $$(PlutusTx.compile [|| mkOrByteStringSucceedingPolicy ||])
     `PlutusTx.unsafeApplyCode` params

writeOrByteStringPolicyScriptsV3 :: IO ()
writeOrByteStringPolicyScriptsV3 =
  writeSerialisedScript "orByteStringSucceedingPolicyV3" orByteStringSucceedingPolicyScriptV3

xorByteStringSucceedingPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
xorByteStringSucceedingPolicyScriptV3 =
  let params = PlutusTx.liftCode plcVersion100 xorByteStringParams
  in C.PlutusScriptSerialised $
     serialiseCompiledCode $ $$(PlutusTx.compile [|| mkXorByteStringSucceedingPolicy ||])
     `PlutusTx.unsafeApplyCode` params

writeXorByteStringPolicyScriptsV3 :: IO ()
writeXorByteStringPolicyScriptsV3 =
  writeSerialisedScript "xorByteStringSucceedingPolicyV3" xorByteStringSucceedingPolicyScriptV3

complementByteStringSucceedingPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
complementByteStringSucceedingPolicyScriptV3 =
  let params = PlutusTx.liftCode plcVersion100 complementByteStringParams
  in C.PlutusScriptSerialised $
     serialiseCompiledCode $ $$(PlutusTx.compile [|| mkComplementByteStringSucceedingPolicy ||])
     `PlutusTx.unsafeApplyCode` params

writeComplementByteStringSucceedingPolicyScriptsV3 :: IO ()
writeComplementByteStringSucceedingPolicyScriptsV3 =
  writeSerialisedScript "complementByteStringSucceedingPolicyV3" complementByteStringSucceedingPolicyScriptV3


