{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Batch6.BLS12_381_MultiScalarMul.G1.V3_100 where

import PlutusCore.Version (plcVersion100)
import PlutusScripts.Batch6.BLS12_381_MultiScalarMul.G1.Common (mkSucceedingMultiScalarMulPolicy, succeedingMultiScalarMulParams)
import PlutusTx (compile, liftCode, unsafeApplyCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as P

-- Compiled code values with parameters already applied for succeeding tests
succeedingMultiScalarMulPolicyScript
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingMultiScalarMulPolicyScript =
  $$(compile [||mkSucceedingMultiScalarMulPolicy||])
    `unsafeApplyCode` liftCode plcVersion100 succeedingMultiScalarMulParams
