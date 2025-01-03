{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PlutusScripts.Helpers where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (void)
import Data.ByteString qualified as BS (ByteString)
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V1.Bytes qualified as P (bytes, fromHex)
import PlutusLedgerApi.V1.Scripts (Datum (Datum), Redeemer (Redeemer))
import PlutusLedgerApi.V1.Value (CurrencySymbol)
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P
import System.Directory (createDirectoryIfMissing)

-- | Treat string of hexadecimal bytes literally, without encoding. Useful for hashes.
bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = P.bytes . fromEither . P.fromHex
  where
    fromEither (Left e) = error $ show e
    fromEither (Right b) = b

-- Helper to reduce clutter
hxs :: BS.ByteString -> P.BuiltinByteString
hxs = BI.toBuiltin . bytesFromHex

{- | Default execution units with zero values. Needed for valid script witness in txbody.
 Useful when exunits are automatically balanced.
-}
defExecutionUnits :: C.ExecutionUnits
defExecutionUnits = C.ExecutionUnits{C.executionSteps = 0, C.executionMemory = 0}

-- | Any data to ScriptData. Used for script datum and redeemer.
toScriptData :: (PlutusTx.ToData a) => a -> C.HashableScriptData
toScriptData = C.unsafeHashableScriptData . C.fromPlutusData . PlutusTx.toData

asRedeemer :: (PlutusTx.ToData a) => a -> Redeemer
asRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

asDatum :: (PlutusTx.ToData a) => a -> Datum
asDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

plutusL1 :: C.ScriptLanguage C.PlutusScriptV1
plutusL1 = C.PlutusScriptLanguage C.PlutusScriptV1

plutusL2 :: C.ScriptLanguage C.PlutusScriptV2
plutusL2 = C.PlutusScriptLanguage C.PlutusScriptV2

plutusL3 :: C.ScriptLanguage C.PlutusScriptV3
plutusL3 = C.PlutusScriptLanguage C.PlutusScriptV3

{- | Witness token mint for including in txbody's txMintValue.
 Provide either the script or TxIn for reference script to include in witness.
 Zero execution units can only be used with convenience build function.
-}
mintScriptWitness
  :: C.ShelleyBasedEra era
  -> C.ScriptLanguage lang
  -> Either (C.PlutusScript lang) C.TxIn -- either script or reference to script
  -> C.HashableScriptData
  -> C.ScriptWitness C.WitCtxMint era
-- V1 script
mintScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV1) eScript redeemer =
  mintScriptWitness' era lang eScript redeemer defExecutionUnits
-- V2 script
mintScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Left script) redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV2
    (C.PScript script)
    C.NoScriptDatumForMint
    redeemer
    defExecutionUnits
-- V2 reference script
mintScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Right refTxIn) redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV2
    (C.PReferenceScript refTxIn Nothing)
    C.NoScriptDatumForMint
    redeemer
    defExecutionUnits
-- V3 script
mintScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV3) (Left script) redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV3
    (C.PScript script)
    C.NoScriptDatumForMint
    redeemer
    defExecutionUnits
-- V3 reference script
mintScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV3) (Right refTxIn) redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV3
    (C.PReferenceScript refTxIn Nothing)
    C.NoScriptDatumForMint
    redeemer
    defExecutionUnits

-- Witness token mint with explicit execution units. Used when building raw txbody content.
mintScriptWitness'
  :: C.ShelleyBasedEra era
  -> C.ScriptLanguage lang
  -> Either (C.PlutusScript lang) C.TxIn -- either script or reference to script
  -> C.HashableScriptData
  -> C.ExecutionUnits
  -> C.ScriptWitness C.WitCtxMint era
-- V1 script
mintScriptWitness' era lang@(C.PlutusScriptLanguage C.PlutusScriptV1) (Left script) redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV1
    (C.PScript script)
    C.NoScriptDatumForMint
    redeemer
-- V2 script
mintScriptWitness' era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Left script) redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV2
    (C.PScript script)
    C.NoScriptDatumForMint
    redeemer
-- V2 reference script
mintScriptWitness' era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Right refTxIn) redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV2
    (C.PReferenceScript refTxIn Nothing)
    C.NoScriptDatumForMint
    redeemer
-- V3 script
mintScriptWitness' era lang@(C.PlutusScriptLanguage C.PlutusScriptV3) (Left script) redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV3
    (C.PScript script)
    C.NoScriptDatumForMint
    redeemer
-- V3 reference script
mintScriptWitness' era lang@(C.PlutusScriptLanguage C.PlutusScriptV3) (Right refTxIn) redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV3
    (C.PReferenceScript refTxIn Nothing)
    C.NoScriptDatumForMint
    redeemer

spendScriptWitness
  :: C.ShelleyBasedEra era
  -> C.ScriptLanguage lang
  -> Either (C.PlutusScript lang) C.TxIn -- either script or reference to script
  -> C.ScriptDatum C.WitCtxTxIn
  -> C.HashableScriptData
  -> C.ScriptWitness C.WitCtxTxIn era
-- V1 script
spendScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV1) (Left script) datumWit redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV1
    (C.PScript script)
    datumWit
    redeemer
    defExecutionUnits
-- V2 script
spendScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Left script) datumWit redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV2
    (C.PScript script)
    datumWit
    redeemer
    defExecutionUnits
-- V2 reference script
spendScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV2) (Right refTxIn) datumWit redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV2
    (C.PReferenceScript refTxIn Nothing)
    datumWit
    redeemer
    defExecutionUnits
-- V3 script
spendScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV3) (Left script) datumWit redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV3
    (C.PScript script)
    datumWit
    redeemer
    defExecutionUnits
-- V3 reference script
spendScriptWitness era lang@(C.PlutusScriptLanguage C.PlutusScriptV3) (Right refTxIn) datumWit redeemer = do
  C.PlutusScriptWitness
    (maybeScriptWitness era lang $ C.scriptLanguageSupportedInEra era lang)
    C.PlutusScriptV3
    (C.PReferenceScript refTxIn Nothing)
    datumWit
    redeemer
    defExecutionUnits

-- | Produce ScriptLanguageInEra. Throw error when era doesn't support the script language.
maybeScriptWitness
  :: C.ShelleyBasedEra era
  -> C.ScriptLanguage l
  -> Maybe (C.ScriptLanguageInEra l era)
  -> C.ScriptLanguageInEra l era
maybeScriptWitness era lang Nothing =
  error $
    "Era "
      ++ show era
      ++ " does not support script language "
      ++ show lang
maybeScriptWitness _ _ (Just p) = p

-- | V1 Plutus Script to general Script, Needed for producing reference script.
unPlutusScriptV1 :: C.PlutusScript C.PlutusScriptV1 -> C.Script C.PlutusScriptV1
unPlutusScriptV1 = C.PlutusScript C.PlutusScriptV1

-- | V2 Plutus Script to general Script, Needed for producing reference script.
unPlutusScriptV2 :: C.PlutusScript C.PlutusScriptV2 -> C.Script C.PlutusScriptV2
unPlutusScriptV2 = C.PlutusScript C.PlutusScriptV2

-- | V3 Plutus Script to general Script, Needed for producing reference script.
unPlutusScriptV3 :: C.PlutusScript C.PlutusScriptV3 -> C.Script C.PlutusScriptV3
unPlutusScriptV3 = C.PlutusScript C.PlutusScriptV3

-- | PolicyId of a V1 minting policy
policyIdV1 :: SerialisedScript -> C.PolicyId
policyIdV1 = C.scriptPolicyId . unPlutusScriptV1 . C.PlutusScriptSerialised

-- | PolicyId of a V2 minting policy
policyIdV2 :: SerialisedScript -> C.PolicyId
policyIdV2 = C.scriptPolicyId . unPlutusScriptV2 . C.PlutusScriptSerialised

-- | PolicyId of a V3 minting policy
policyIdV3 :: SerialisedScript -> C.PolicyId
policyIdV3 = C.scriptPolicyId . unPlutusScriptV3 . C.PlutusScriptSerialised

fromPolicyId :: C.PolicyId -> CurrencySymbol
fromPolicyId (C.PolicyId hash) = PlutusV1.CurrencySymbol . BI.toBuiltin $ C.serialiseToRawBytes hash

writeSerialisedScript :: (C.HasTextEnvelope ps) => FilePath -> ps -> IO ()
writeSerialisedScript filename plutusScript = do
  let dir = "serialised-plutus-scripts"
      file = C.File $ dir ++ "/" ++ filename ++ ".plutus"
  createDirectoryIfMissing True dir
  void $ C.writeFileTextEnvelope file Nothing plutusScript
