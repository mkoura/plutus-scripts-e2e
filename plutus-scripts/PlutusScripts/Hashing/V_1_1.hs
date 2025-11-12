{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Hashing.V_1_1 where

import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.Hashing.Common (hashingParamsV3, mkHashingPolicyV3)
import PlutusScripts.Hashing.Ripemd_160 (mkRipemd_160Policy, succeedingRipemd_160Params)
import PlutusTx (compile, liftCode, unsafeApplyCode)
import PlutusTx.Code (CompiledCodeIn)
import PlutusTx.Prelude qualified as P

checkHashingPolicy :: V3.SerialisedScript
checkHashingPolicy =
  V3.serialiseCompiledCode $
    $$(compile [||mkHashingPolicyV3||])
      `unsafeApplyCode` liftCode plcVersion110 hashingParamsV3

-- A separate test for ripemd_160 since that only appeared in PlutusV3

succeedingRipemd_160PolicyCompiled
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingRipemd_160PolicyCompiled =
  $$(compile [||mkRipemd_160Policy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingRipemd_160Params
