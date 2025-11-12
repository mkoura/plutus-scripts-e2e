{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.Vrf.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.BLS.Vrf.Common (verifyBlsVrfScript)
import PlutusTx qualified

verifyBlsVrfPolicyV3 :: SerialisedScript
verifyBlsVrfPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||mkUntypedMintingPolicy @V3.ScriptContext verifyBlsVrfScript||])
