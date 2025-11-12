{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.SimpleSignAndVerify.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.BLS.SimpleSignAndVerify.Common (verifyBlsSimpleScript)
import PlutusTx qualified

verifyBlsSimplePolicyV3 :: SerialisedScript
verifyBlsSimplePolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||mkUntypedMintingPolicy @V3.ScriptContext verifyBlsSimpleScript||])
