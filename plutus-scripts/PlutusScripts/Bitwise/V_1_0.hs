{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Bitwise.V_1_0 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Bitwise.Common (
  mkIntegerToByteStringPolicySimple,
 )
import PlutusScripts.Helpers (
  writeSerialisedScript,
 )
import PlutusTx qualified

-- Integer to ByteString added to PlutusV2 --

integerToByteStringPolicyV2 :: SerialisedScript
integerToByteStringPolicyV2 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||mkIntegerToByteStringPolicySimple||])

integerToByteStringPolicyScriptV2 :: C.PlutusScript C.PlutusScriptV2
integerToByteStringPolicyScriptV2 = C.PlutusScriptSerialised integerToByteStringPolicyV2

writeIntegerToByteStringPolicyScriptV2 :: IO ()
writeIntegerToByteStringPolicyScriptV2 = writeSerialisedScript "integerToByteStringPolicyV2" integerToByteStringPolicyScriptV2
