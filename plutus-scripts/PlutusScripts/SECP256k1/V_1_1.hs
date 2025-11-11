{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.SECP256k1.V_1_1 where

import Cardano.Api qualified as C
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))
import PlutusLedgerApi.V3 qualified as PlutusV3 (serialiseCompiledCode)
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL3,
  policyIdV3,
  writeCompiledScript,
 )
import PlutusScripts.SECP256k1.Common (
  ecdsaAssetName,
  mkVerifyEcdsaPolicy,
  mkVerifyEcdsaPolicyV3,
  mkVerifySchnorrPolicy,
  mkVerifySchnorrPolicyV3,
  schnorrAssetName,
  verifyEcdsaParams,
  verifyEcdsaRedeemer,
  verifySchnorrParams,
  verifySchnorrRedeemer,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

-- Schnorr minting policy --

-- verifySchnorrPolicy :: SerialisedScript
-- verifySchnorrPolicy = serialiseCompiledCode $$(PlutusTx.compile [||wrap||])
--   where
--     wrap = mkUntypedMintingPolicy mkVerifySchnorrPolicy

verifySchnorrPolicy :: SerialisedScript
verifySchnorrPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkVerifySchnorrPolicy||])
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 verifySchnorrParams)

verifySchnorrPolicyCompiledV3
  :: PlutusTx.CompiledCode (P.BuiltinData -> P.BuiltinUnit)
verifySchnorrPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkVerifySchnorrPolicyV3||])

verifySchnorrPolicyV3 :: SerialisedScript
verifySchnorrPolicyV3 =
  PlutusV3.serialiseCompiledCode verifySchnorrPolicyCompiledV3

verifySchnorrPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifySchnorrPolicyScriptV3 = C.PlutusScriptSerialised verifySchnorrPolicyV3

writeVerifySchnorrPolicyScriptV3 :: IO ()
writeVerifySchnorrPolicyScriptV3 =
  writeCompiledScript
    PlutusV3
    "verifySchnorrPolicyScriptV3"
    verifySchnorrPolicyCompiledV3

verifySchnorrAssetIdV3 :: C.AssetId
verifySchnorrAssetIdV3 = C.AssetId (policyIdV3 verifySchnorrPolicy) schnorrAssetName

verifySchnorrMintWitnessV3 ::
  C.ShelleyBasedEra era ->
  (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifySchnorrMintWitnessV3 era =
  ( policyIdV3 verifySchnorrPolicy
  , mintScriptWitness era plutusL3 (Left verifySchnorrPolicyScriptV3) verifySchnorrRedeemer
  )

-- ECDSA minting policy --

-- verifyEcdsaPolicy :: SerialisedScript
-- verifyEcdsaPolicy = serialiseCompiledCode $$(PlutusTx.compile [||wrap||])
--   where
--     wrap = mkUntypedMintingPolicy mkVerifyEcdsaPolicy

verifyEcdsaPolicy :: SerialisedScript
verifyEcdsaPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkVerifyEcdsaPolicy||])
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 verifyEcdsaParams)

verifyEcdsaPolicyCompiledV3
  :: PlutusTx.CompiledCode (P.BuiltinData -> P.BuiltinUnit)
verifyEcdsaPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkVerifyEcdsaPolicyV3||])

verifyEcdsaPolicyV3 :: SerialisedScript
verifyEcdsaPolicyV3 =
  PlutusV3.serialiseCompiledCode verifyEcdsaPolicyCompiledV3

verifyEcdsaPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
verifyEcdsaPolicyScriptV3 = C.PlutusScriptSerialised verifyEcdsaPolicyV3

writeVerifyEcdsaPolicyScriptV3 :: IO ()
writeVerifyEcdsaPolicyScriptV3 =
  writeCompiledScript
    PlutusV3
    "verifyEcdsaPolicyScriptV3"
    verifyEcdsaPolicyCompiledV3

verifyEcdsaAssetIdV3 :: C.AssetId
verifyEcdsaAssetIdV3 = C.AssetId (policyIdV3 verifyEcdsaPolicy) ecdsaAssetName

verifyEcdsaMintWitnessV3 ::
  C.ShelleyBasedEra era ->
  (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
verifyEcdsaMintWitnessV3 sbe =
  ( policyIdV3 verifyEcdsaPolicy
  , mintScriptWitness sbe plutusL3 (Left verifyEcdsaPolicyScriptV3) verifyEcdsaRedeemer
  )
