{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.VerifyOverG1.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.BLS.Common (blsSigBls12381G2XmdSha256SswuRoNul)
import PlutusScripts.BLS.VerifyOverG1.Common (verifySigG1Script)
import PlutusTx qualified

verifyBlsSigG1PolicyV3 :: SerialisedScript
verifyBlsSigG1PolicyV3 =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkUntypedMintingPolicy @PlutusV3.ScriptContext . verifySigG1Script||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 blsSigBls12381G2XmdSha256SswuRoNul
