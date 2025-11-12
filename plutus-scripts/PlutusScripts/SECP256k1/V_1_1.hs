{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.SECP256k1.V_1_1 where

import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))
import PlutusLedgerApi.V3 qualified as PlutusV3 (serialiseCompiledCode)
import PlutusScripts.Helpers (writeCompiledScript)
import PlutusScripts.SECP256k1.Common (
  mkVerifyEcdsaPolicy,
  mkVerifyEcdsaPolicyV3,
  mkVerifySchnorrPolicy,
  mkVerifySchnorrPolicyV3,
  verifyEcdsaParams,
  verifySchnorrParams,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

-- Schnorr minting policy --

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

writeVerifySchnorrPolicyScriptV3 :: IO ()
writeVerifySchnorrPolicyScriptV3 =
  writeCompiledScript
    PlutusV3
    "verifySchnorrPolicyScriptV3"
    verifySchnorrPolicyCompiledV3

-- ECDSA minting policy --

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

writeVerifyEcdsaPolicyScriptV3 :: IO ()
writeVerifyEcdsaPolicyScriptV3 =
  writeCompiledScript
    PlutusV3
    "verifyEcdsaPolicyScriptV3"
    verifyEcdsaPolicyCompiledV3
