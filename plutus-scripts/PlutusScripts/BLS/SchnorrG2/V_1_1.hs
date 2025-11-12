{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.SchnorrG2.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.BLS.Common (byteString16Null)
import PlutusScripts.BLS.SchnorrG2.Common (verifySchnorrG2Script)
import PlutusTx (compile, liftCode, unsafeApplyCode)

verifyBlsSchnorrG2PolicyV3 :: V3.SerialisedScript
verifyBlsSchnorrG2PolicyV3 =
  V3.serialiseCompiledCode $
    $$(compile [||mkUntypedMintingPolicy @V3.ScriptContext . verifySchnorrG2Script||])
      `unsafeApplyCode` liftCode plcVersion110 byteString16Null
