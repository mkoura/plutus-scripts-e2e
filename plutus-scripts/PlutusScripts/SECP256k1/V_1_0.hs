{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.SECP256k1.V_1_0 where

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.SECP256k1.Common (
  mkVerifyEcdsaPolicy,
  mkVerifySchnorrPolicy,
  verifyEcdsaParams,
  verifySchnorrParams,
 )
import PlutusTx qualified

-- Schnorr minting policy --

verifySchnorrPolicy :: SerialisedScript
verifySchnorrPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkVerifySchnorrPolicy||])
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion100 verifySchnorrParams)

-- ECDSA minting policy --

verifyEcdsaPolicy :: SerialisedScript
verifyEcdsaPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkVerifyEcdsaPolicy||])
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion100 verifyEcdsaParams)
