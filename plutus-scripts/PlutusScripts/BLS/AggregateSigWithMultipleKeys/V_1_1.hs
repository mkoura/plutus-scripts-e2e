{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.AggregateSigWithMultipleKeys.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.BLS.AggregateSigWithMultipleKeys.Common (aggregateMultiKeyG2Script)
import PlutusScripts.BLS.Common (blsSigBls12381G2XmdSha256SswuRoNul, byteString16Null)
import PlutusTx qualified

verifyBlsAggregateSigMultiKeyG2PolicyV3 :: SerialisedScript
verifyBlsAggregateSigMultiKeyG2PolicyV3 =
    serialiseCompiledCode $
        $$(PlutusTx.compile [||\a b -> mkUntypedMintingPolicy @PlutusV3.ScriptContext (aggregateMultiKeyG2Script a b)||])
            `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 byteString16Null
            `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 blsSigBls12381G2XmdSha256SswuRoNul
