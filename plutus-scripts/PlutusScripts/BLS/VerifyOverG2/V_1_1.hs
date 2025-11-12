{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.VerifyOverG2.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.BLS.Common (blsSigBls12381G2XmdSha256SswuRoNul)
import PlutusScripts.BLS.VerifyOverG2.Common (verifySigG2Script)
import PlutusTx (compile, liftCode, unsafeApplyCode)

verifyBlsSigG2PolicyV3 :: V3.SerialisedScript
verifyBlsSigG2PolicyV3 =
  V3.serialiseCompiledCode $
    $$(compile [||mkUntypedMintingPolicy @V3.ScriptContext . verifySigG2Script||])
      `unsafeApplyCode` liftCode plcVersion110 blsSigBls12381G2XmdSha256SswuRoNul
