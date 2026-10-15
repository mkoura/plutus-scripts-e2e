{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Batch6.BLS12_381_MultiScalarMul.G2.V3_100 where

import PlutusCore.Version (plcVersion100)
import PlutusScripts.Batch6.BLS12_381_MultiScalarMul.G2.Common (
  mkSucceedingMultiScalarMulPolicy1,
  mkSucceedingMultiScalarMulPolicy2,
  succeedingMultiScalarMulParams1a,
  succeedingMultiScalarMulParams1b,
  succeedingMultiScalarMulParams2a,
  succeedingMultiScalarMulParams2b,
 )
import PlutusTx (compile, liftCode, unsafeApplyCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as P

-- Compiled code values with parameters already applied for succeeding tests
succeedingMultiScalarMulPolicyScript1a
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingMultiScalarMulPolicyScript1a =
  $$(compile [||mkSucceedingMultiScalarMulPolicy1||])
    `unsafeApplyCode` liftCode plcVersion100 succeedingMultiScalarMulParams1a

succeedingMultiScalarMulPolicyScript1b
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingMultiScalarMulPolicyScript1b =
  $$(compile [||mkSucceedingMultiScalarMulPolicy1||])
    `unsafeApplyCode` liftCode plcVersion100 succeedingMultiScalarMulParams1b

succeedingMultiScalarMulPolicyScript2a
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingMultiScalarMulPolicyScript2a =
  $$(compile [||mkSucceedingMultiScalarMulPolicy2||])
    `unsafeApplyCode` liftCode plcVersion100 succeedingMultiScalarMulParams2a

succeedingMultiScalarMulPolicyScript2b
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingMultiScalarMulPolicyScript2b =
  $$(compile [||mkSucceedingMultiScalarMulPolicy2||])
    `unsafeApplyCode` liftCode plcVersion100 succeedingMultiScalarMulParams2b
