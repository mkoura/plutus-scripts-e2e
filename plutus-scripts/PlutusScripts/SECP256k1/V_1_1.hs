{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.SECP256k1.V_1_1 where

import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.SECP256k1.Common (
  mkVerifyEcdsaPolicy,
  mkVerifyEcdsaPolicyV3,
  mkVerifySchnorrPolicy,
  mkVerifySchnorrPolicyV3,
  verifyEcdsaParams,
  verifySchnorrParams,
 )
import PlutusTx (CompiledCode, compile, liftCode, unsafeApplyCode)
import PlutusTx.Prelude qualified as P

-- Schnorr minting policy --

verifySchnorrPolicy :: V3.SerialisedScript
verifySchnorrPolicy =
  V3.serialiseCompiledCode $
    $$(compile [||mkVerifySchnorrPolicy||])
      `unsafeApplyCode` liftCode plcVersion110 verifySchnorrParams

verifySchnorrPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
verifySchnorrPolicyCompiledV3 =
  $$(compile [||mkVerifySchnorrPolicyV3||])

verifySchnorrPolicyV3 :: V3.SerialisedScript
verifySchnorrPolicyV3 =
  V3.serialiseCompiledCode verifySchnorrPolicyCompiledV3

-- ECDSA minting policy --

verifyEcdsaPolicy :: V3.SerialisedScript
verifyEcdsaPolicy =
  V3.serialiseCompiledCode $
    $$(compile [||mkVerifyEcdsaPolicy||])
      `unsafeApplyCode` liftCode plcVersion110 verifyEcdsaParams

verifyEcdsaPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
verifyEcdsaPolicyCompiledV3 =
  $$(compile [||mkVerifyEcdsaPolicyV3||])

verifyEcdsaPolicyV3 :: V3.SerialisedScript
verifyEcdsaPolicyV3 =
  V3.serialiseCompiledCode verifyEcdsaPolicyCompiledV3
