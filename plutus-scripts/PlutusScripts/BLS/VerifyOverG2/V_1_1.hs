{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.VerifyOverG2.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.BLS.Common (blsSigBls12381G2XmdSha256SswuRoNul)
import PlutusScripts.BLS.VerifyOverG2.Common (verifySigG2Script)
import PlutusTx qualified

verifyBlsSigG2PolicyV3 :: SerialisedScript
verifyBlsSigG2PolicyV3 =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkUntypedMintingPolicy @V3.ScriptContext . verifySigG2Script||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 blsSigBls12381G2XmdSha256SswuRoNul
