{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- preserves traces
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Basic.V_1_1 where

import Cardano.Api qualified as C
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1 (Redeemer, ScriptPurpose (Minting))
import PlutusLedgerApi.V2 qualified as PlutusV2 (Map)
import PlutusLedgerApi.V3 qualified as PlutusV3 (serialiseCompiledCode)
import PlutusScripts.Basic.Common (
  mkAlwaysFailsPolicyV3,
  mkAlwaysSucceedPolicyV3,
  mkAlwaysSucceedSpend,
  mkMintTokenNamePolicyV3,
  mkTimeRangePolicyV3,
  mkWitnessRedeemerPolicyV3,
 )
import PlutusScripts.Helpers (
  asRedeemer,
  emptyAssetName,
  fromPolicyId,
  mintScriptWitness,
  mintScriptWitness',
  plutusL3,
  policyIdV3,
  spendScriptWitness,
  toScriptData,
  writeSerialisedScript,
 )
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AMap

-- AlwaysSucceeds minting policy --

alwaysSucceedPolicy :: SerialisedScript
alwaysSucceedPolicy = serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysSucceedPolicyV3||])

alwaysSucceedPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
alwaysSucceedPolicyScriptV3 = C.PlutusScriptSerialised alwaysSucceedPolicy

writeAlwaysSucceedPolicyScriptV3 :: IO ()
writeAlwaysSucceedPolicyScriptV3 = writeSerialisedScript "alwaysSucceedPolicyScriptV3" alwaysSucceedPolicyScriptV3

alwaysSucceedPolicyIdV3 :: C.PolicyId
alwaysSucceedPolicyIdV3 = policyIdV3 alwaysSucceedPolicy

alwaysSucceedAssetIdV3 :: C.AssetId
alwaysSucceedAssetIdV3 = C.AssetId (policyIdV3 alwaysSucceedPolicy) emptyAssetName

alwaysSucceedPolicyTxInfoRedeemerV3 :: PlutusV2.Map ScriptPurpose Redeemer
alwaysSucceedPolicyTxInfoRedeemerV3 =
  AMap.singleton
    (Minting $ fromPolicyId alwaysSucceedPolicyIdV3)
    (asRedeemer $ PlutusTx.toBuiltinData ())

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysSucceedMintWitnessV3 ::
  C.ShelleyBasedEra era ->
  Maybe C.TxIn -> -- maybe reference input
  (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV3 sbe Nothing =
  ( policyIdV3 alwaysSucceedPolicy
  , mintScriptWitness sbe plutusL3 (Left alwaysSucceedPolicyScriptV3) (toScriptData ())
  )
alwaysSucceedMintWitnessV3 sbe (Just refTxIn) =
  ( policyIdV3 alwaysSucceedPolicy
  , mintScriptWitness sbe plutusL3 (Right refTxIn) (toScriptData ())
  )

alwaysSucceedMintWitnessV3' ::
  C.ShelleyBasedEra era ->
  C.ExecutionUnits ->
  (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysSucceedMintWitnessV3' sbe exunits =
  ( policyIdV3 alwaysSucceedPolicy
  , mintScriptWitness' sbe plutusL3 (Left alwaysSucceedPolicyScriptV3) (toScriptData ()) exunits
  )

-- AlwaysSucceeds validator --

alwaysSucceedSpend :: SerialisedScript
alwaysSucceedSpend = serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysSucceedSpend||])

alwaysSucceedSpendScriptV3 :: C.PlutusScript C.PlutusScriptV3
alwaysSucceedSpendScriptV3 = C.PlutusScriptSerialised alwaysSucceedSpend

-- writeAlwaysSucceedSpendScriptV3 :: IO ()
-- writeAlwaysSucceedSpendScriptV3 = writeSerialisedScript "alwaysSucceedSpendScriptV3" alwaysSucceedSpendScriptV3

alwaysSucceedSpendScriptHashV3 :: C.ScriptHash
alwaysSucceedSpendScriptHashV3 = C.hashScript $ C.PlutusScript C.PlutusScriptV3 alwaysSucceedSpendScriptV3

alwaysSucceedSpendWitnessV3 ::
  C.ShelleyBasedEra era ->
  Maybe C.TxIn ->
  Maybe C.HashableScriptData ->
  C.Witness C.WitCtxTxIn era
alwaysSucceedSpendWitnessV3 era mRefScript mDatum =
  C.ScriptWitness C.ScriptWitnessForSpending $
    spendScriptWitness
      era
      plutusL3
      (maybe (Left alwaysSucceedSpendScriptV3) Right mRefScript) -- script or reference script
      (maybe C.InlineScriptDatum (C.ScriptDatumForTxIn . pure) mDatum) -- inline datum or datum value
      (toScriptData ()) -- redeemer

-- AlwaysFails minting policy --

alwaysFailsPolicy :: SerialisedScript
alwaysFailsPolicy = serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysFailsPolicyV3||])

alwaysFailsPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
alwaysFailsPolicyScriptV3 = C.PlutusScriptSerialised alwaysFailsPolicy

writeAlwaysFailsPolicyScriptV3 :: IO ()
writeAlwaysFailsPolicyScriptV3 = writeSerialisedScript "alwaysFailsPolicyScriptV3" alwaysFailsPolicyScriptV3

alwaysFailsPolicyIdV3 :: C.PolicyId
alwaysFailsPolicyIdV3 = policyIdV3 alwaysFailsPolicy

alwaysFailsAssetIdV3 :: C.AssetId
alwaysFailsAssetIdV3 = C.AssetId alwaysFailsPolicyIdV3 emptyAssetName

alwaysFailsPolicyTxInfoRedeemerV3 :: PlutusV2.Map ScriptPurpose Redeemer
alwaysFailsPolicyTxInfoRedeemerV3 =
  AMap.singleton
    (Minting $ fromPolicyId alwaysFailsPolicyIdV3)
    (asRedeemer $ PlutusTx.toBuiltinData ())

{- | Witness token mint for including in txbody's txMintValue
Use Nothing to include script in witness, else provide TxIn to reference script
-}
alwaysFailsMintWitnessV3 ::
  C.ShelleyBasedEra era ->
  Maybe C.TxIn -> -- maybe reference input
  (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV3 sbe Nothing =
  ( policyIdV3 alwaysFailsPolicy
  , mintScriptWitness sbe plutusL3 (Left alwaysFailsPolicyScriptV3) (toScriptData ())
  )
alwaysFailsMintWitnessV3 sbe (Just refTxIn) =
  ( policyIdV3 alwaysFailsPolicy
  , mintScriptWitness sbe plutusL3 (Right refTxIn) (toScriptData ())
  )

alwaysFailsMintWitnessV3' ::
  C.ShelleyBasedEra era ->
  C.ExecutionUnits ->
  (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
alwaysFailsMintWitnessV3' sbe exunits =
  ( policyIdV3 alwaysFailsPolicy
  , mintScriptWitness' sbe plutusL3 (Left alwaysFailsPolicyScriptV3) (toScriptData ()) exunits
  )

-- Mint token name policy --

mintTokenNamePolicyV3 :: SerialisedScript
mintTokenNamePolicyV3 = PlutusV3.serialiseCompiledCode $$(PlutusTx.compile [||mkMintTokenNamePolicyV3||])

mintTokenNamePolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
mintTokenNamePolicyScriptV3 = C.PlutusScriptSerialised mintTokenNamePolicyV3

writeTokenNamePolicyScriptV3 :: IO ()
writeTokenNamePolicyScriptV3 = writeSerialisedScript "mintTokenNamePolicyScriptV3" mintTokenNamePolicyScriptV3

-- Time range policy --

timeRangePolicyV3 :: SerialisedScript
timeRangePolicyV3 = PlutusV3.serialiseCompiledCode $$(PlutusTx.compile [||mkTimeRangePolicyV3||])

timeRangePolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
timeRangePolicyScriptV3 = C.PlutusScriptSerialised timeRangePolicyV3

writeTimeRangePolicyScriptV3 :: IO ()
writeTimeRangePolicyScriptV3 = writeSerialisedScript "timeRangePolicyScriptV3" timeRangePolicyScriptV3

-- Witness redeemer policy --

witnessRedeemerPolicyV3 :: SerialisedScript
witnessRedeemerPolicyV3 = PlutusV3.serialiseCompiledCode $$(PlutusTx.compile [||mkWitnessRedeemerPolicyV3||])

witnessRedeemerPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
witnessRedeemerPolicyScriptV3 = C.PlutusScriptSerialised witnessRedeemerPolicyV3

writeWitnessRedeemerPolicyScriptV3 :: IO ()
writeWitnessRedeemerPolicyScriptV3 = writeSerialisedScript "witnessRedeemerPolicyScriptV3" witnessRedeemerPolicyScriptV3
