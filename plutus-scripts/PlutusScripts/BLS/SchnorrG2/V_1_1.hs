{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.SchnorrG2.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Core qualified as PLC
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.BLS.Common (byteString16Null)
import PlutusScripts.BLS.SchnorrG2.Common (verifySchnorrG2Script)
import PlutusTx qualified

verifyBlsSchnorrG2PolicyV3 :: SerialisedScript
verifyBlsSchnorrG2PolicyV3 =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkUntypedMintingPolicy @V3.ScriptContext . verifySchnorrG2Script||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode PLC.plcVersion110 byteString16Null
