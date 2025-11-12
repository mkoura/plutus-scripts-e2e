{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Hashing.V_1_0 where

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Hashing.Common (
  hashingParamsV1V2,
  mkHashingPolicyV1V2,
 )
import PlutusTx qualified

checkHashingPolicy :: SerialisedScript
checkHashingPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkHashingPolicyV1V2||])
      `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 hashingParamsV1V2
