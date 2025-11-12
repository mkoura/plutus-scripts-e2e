{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Hashing.V_1_1 where

import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Hashing.Common (hashingParamsV3, mkHashingPolicyV3)
import PlutusScripts.Hashing.Ripemd_160 (mkRipemd_160Policy, succeedingRipemd_160Params)
import PlutusTx qualified
import PlutusTx.Code (CompiledCodeIn)
import PlutusTx.Prelude qualified as P

checkHashingPolicy :: SerialisedScript
checkHashingPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkHashingPolicyV3||])
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 hashingParamsV3)

-- A separate test for ripemd_160 since that only appeared in PlutusV3

succeedingRipemd_160PolicyCompiled
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingRipemd_160PolicyCompiled =
  $$(PlutusTx.compile [||mkRipemd_160Policy||])
    `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 succeedingRipemd_160Params)
