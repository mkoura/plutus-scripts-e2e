{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Batch6.BLS12_381_MultiScalarMul.G1.V3_110 where

import PlutusCore.Version (plcVersion110)
import PlutusScripts.Batch6.BLS12_381_MultiScalarMul.G1.Common (
  mkSucceedingMultiScalarMulPolicy1,
  mkSucceedingMultiScalarMulPolicy2,
  succeedingMultiScalarMulParams1,
  succeedingMultiScalarMulParams2,
 )
import PlutusTx (compile, liftCode, unsafeApplyCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as P

-- Compiled code values with parameters already applied for succeeding tests
succeedingMultiScalarMulPolicyScript1
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingMultiScalarMulPolicyScript1 =
  $$(compile [||mkSucceedingMultiScalarMulPolicy1||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingMultiScalarMulParams1

succeedingMultiScalarMulPolicyScript2
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingMultiScalarMulPolicyScript2 =
  $$(compile [||mkSucceedingMultiScalarMulPolicy2||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingMultiScalarMulParams2
