{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.AggregateSigWithSingleKey.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.BLS.AggregateSigWithSingleKey.Common (aggregateSigSingleKeyG1)
import PlutusScripts.BLS.Common (blsSigBls12381G2XmdSha256SswuRoNul)
import PlutusTx qualified

verifyAggregateSigSingleKeyG1PolicyV3 :: SerialisedScript
verifyAggregateSigSingleKeyG1PolicyV3 =
    serialiseCompiledCode $
        $$(PlutusTx.compile [||mkUntypedMintingPolicy @PlutusV3.ScriptContext . aggregateSigSingleKeyG1||])
            `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 blsSigBls12381G2XmdSha256SswuRoNul
