{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.SchnorrG1.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.BLS.Common (byteString16Null)
import PlutusScripts.BLS.SchnorrG1.Common (verifySchnorrG1Script)
import PlutusTx (compile, liftCode, unsafeApplyCode)

verifyBlsSchnorrG1PolicyV3 :: V3.SerialisedScript
verifyBlsSchnorrG1PolicyV3 =
  V3.serialiseCompiledCode $
    $$(compile [||mkUntypedMintingPolicy @V3.ScriptContext . verifySchnorrG1Script||])
      `unsafeApplyCode` liftCode plcVersion110 byteString16Null
