{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Bitwise.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Version (plcVersion110)
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.Bitwise.Complement (
  mkComplementByteStringPolicy,
  succeedingComplementByteStringParams
  )
import PlutusScripts.Bitwise.CountFindFirstSet (
  mkCountSetBitsPolicy,
  mkFindFirstSetBitPolicy,
  succeedingCountSetBitsParams,
  succeedingFindFirstSetBitParams
  )
import PlutusScripts.Bitwise.Conversions (
    ByteStringToIntegerParams,
    IntegerToByteStringParams,
    bitwiseAssetName,
    mkByteStringToIntegerPolicy,
    mkByteStringToIntegerRoundtripPolicy,
    mkIntegerToByteStringPolicy
  )
import PlutusScripts.Bitwise.Logical (
  mkAndByteStringPolicy,
  mkOrByteStringPolicy,
  mkXorByteStringPolicy,
  succeedingAndByteStringParams,
  succeedingOrByteStringParams,
  succeedingXorByteStringParams
  )
import PlutusScripts.Bitwise.ShiftRotate (
  mkRotateByteStringPolicy,
  mkShiftByteStringPolicy,
  succeedingRotateByteStringParams,
  succeedingShiftByteStringParams
  )
import PlutusScripts.Bitwise.ReadBit (
  mkReadBitPolicy,
  failingReadBitParams,
  succeedingReadBitParams
  )
import PlutusScripts.Bitwise.ReplicateByte (
  mkReplicateBytePolicy,
  failingReplicateByteParams,
  succeedingReplicateByteParams
  )
import PlutusScripts.Bitwise.WriteBits (
  mkWriteBitsPolicy,
  failingWriteBitsParams,
  succeedingWriteBitsParams
  )
import PlutusScripts.Helpers (mintScriptWitness, plutusL3, policyIdV3, toScriptData,
                              writeSerialisedScript)
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

-- ByteString to Integer --

byteStringToIntegerPolicyV3 :: SerialisedScript
byteStringToIntegerPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkByteStringToIntegerPolicy

byteStringToIntegerPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
byteStringToIntegerPolicyScriptV3 = C.PlutusScriptSerialised byteStringToIntegerPolicyV3

byteStringToIntegerAssetIdV3 :: C.AssetId
byteStringToIntegerAssetIdV3 = C.AssetId (policyIdV3 byteStringToIntegerPolicyV3) bitwiseAssetName

byteStringToIntegerMintWitnessV3
  :: C.ShelleyBasedEra era
  -> ByteStringToIntegerParams
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
byteStringToIntegerMintWitnessV3 sbe redeemer =
  ( policyIdV3 byteStringToIntegerPolicyV3
  , mintScriptWitness sbe plutusL3 (Left byteStringToIntegerPolicyScriptV3) (toScriptData redeemer)
  )

-- Integer to ByteString --

integerToByteStringPolicyV3 :: SerialisedScript
integerToByteStringPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkIntegerToByteStringPolicy

integerToByteStringPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
integerToByteStringPolicyScriptV3 = C.PlutusScriptSerialised integerToByteStringPolicyV3

integerToByteStringAssetIdV3 :: C.AssetId
integerToByteStringAssetIdV3 = C.AssetId (policyIdV3 integerToByteStringPolicyV3) bitwiseAssetName

integerToByteStringMintWitnessV3
  :: C.ShelleyBasedEra era
  -> IntegerToByteStringParams
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
integerToByteStringMintWitnessV3 sbe redeemer =
  ( policyIdV3 integerToByteStringPolicyV3
  , mintScriptWitness sbe plutusL3 (Left integerToByteStringPolicyScriptV3) (toScriptData redeemer)
  )

-- ByteString to Integer and Integer to ByteString Roundtrip --

byteStringToIntegerRoundtripPolicyV3 :: SerialisedScript
byteStringToIntegerRoundtripPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkByteStringToIntegerRoundtripPolicy

byteStringToIntegerRoundtripPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
byteStringToIntegerRoundtripPolicyScriptV3 = C.PlutusScriptSerialised byteStringToIntegerRoundtripPolicyV3

byteStringToIntegerRoundtripAssetIdV3 :: C.AssetId
byteStringToIntegerRoundtripAssetIdV3 = C.AssetId (policyIdV3 byteStringToIntegerRoundtripPolicyV3) bitwiseAssetName

byteStringToIntegerAndBackMintWitnessV3
  :: C.ShelleyBasedEra era
  -> P.BuiltinByteString
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
byteStringToIntegerAndBackMintWitnessV3 sbe redeemer =
  ( policyIdV3 byteStringToIntegerRoundtripPolicyV3
  , mintScriptWitness
      sbe
      plutusL3
      (Left byteStringToIntegerRoundtripPolicyScriptV3)
      (toScriptData redeemer)
  )

-- Simple end-to-end tests for bitwise builtins in PlutusV3.  All of these are
-- self-contained: the inputs are compiled into the script rather than being
-- obtained from a redeemer.

-- Each script takes a list of inputs and expected results and iterates over
-- them, checking that the given inputs produce the expected result.
writeSuceedingV3Script
  :: PlutusTx.Lift DefaultUni [param]
  => String
  -> (PlutusTx.CompiledCodeIn  DefaultUni DefaultFun ([param] -> r))
  -> [param]
  -> IO ()
writeSuceedingV3Script name code params =
  let script :: C.PlutusScript C.PlutusScriptV3
      script = C.PlutusScriptSerialised $ serialiseCompiledCode (code `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 params))
  in writeSerialisedScript name script

-- This takes a list of inputs which are expected to cauase a failure.  For
-- failing tests, we have to produce a separate script for every set of inputs
-- because we want to check that *all* cases fail.  We re-use the same script as
-- for succeeding inputs but supply it with a list containing a single set of
-- inputs.
writeFailingV3Scripts
  :: PlutusTx.Lift DefaultUni [param]
  => String
  -> (PlutusTx.CompiledCodeIn  DefaultUni DefaultFun ([param] -> r))
  -> [param]
  -> IO ()
writeFailingV3Scripts name code params =
  let writeOneScript (n::Integer, param) =
        let script :: C.PlutusScript C.PlutusScriptV3
            script = C.PlutusScriptSerialised $
              serialiseCompiledCode (code `PlutusTx.unsafeApplyCode`
                                      (PlutusTx.liftCode plcVersion110 [param]))
        in writeSerialisedScript (name ++ "_" ++ show n) script
  in mapM_ writeOneScript $ zip [1..] params

writeAndByteStringPolicyScriptsV3 :: IO ()
writeAndByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingAndByteStringPolicyScriptV3"
    $$(PlutusTx.compile [|| mkAndByteStringPolicy ||])
    succeedingAndByteStringParams

writeOrByteStringPolicyScriptsV3 :: IO ()
writeOrByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingByteStringPolicyScriptV3"
    $$(PlutusTx.compile [|| mkOrByteStringPolicy ||])
    succeedingOrByteStringParams

writeXorByteStringPolicyScriptsV3 :: IO ()
writeXorByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingXorByteStringPolicyScriptV3"
    $$(PlutusTx.compile [|| mkXorByteStringPolicy ||])
    succeedingXorByteStringParams

writeComplementByteStringPolicyScriptsV3 :: IO ()
writeComplementByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingComplementByteStringPolicyScriptV3"
    $$(PlutusTx.compile [|| mkComplementByteStringPolicy ||])
    succeedingComplementByteStringParams

writeShiftByteStringPolicyScriptsV3 :: IO ()
writeShiftByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingShiftByteStringPolicyScriptV3"
    $$(PlutusTx.compile [|| mkShiftByteStringPolicy ||])
    succeedingShiftByteStringParams

writeRotateByteStringPolicyScriptsV3 :: IO ()
writeRotateByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingRotateByteStringPolicyScriptV3"
    $$(PlutusTx.compile [|| mkRotateByteStringPolicy ||])
    succeedingRotateByteStringParams

writeCountSetBitsPolicyScriptsV3 :: IO ()
writeCountSetBitsPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingCountSetBitsPolicyScriptV3"
    $$(PlutusTx.compile [|| mkCountSetBitsPolicy ||])
    succeedingCountSetBitsParams

writeFindFirstSetBitPolicyScriptsV3 :: IO ()
writeFindFirstSetBitPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingFindFirstSetBitPolicyScriptV3"
    $$(PlutusTx.compile [|| mkFindFirstSetBitPolicy ||])
    succeedingFindFirstSetBitParams

writeReadBitPolicyScriptsV3 :: IO ()
writeReadBitPolicyScriptsV3 = do
  writeSuceedingV3Script
    "succeedingReadBitPolicyScriptV3"
    $$(PlutusTx.compile [|| mkReadBitPolicy ||])
    succeedingReadBitParams
  writeFailingV3Scripts
    "failingReadBitPolicyScriptV3"
    $$(PlutusTx.compile [|| mkReadBitPolicy ||])
    failingReadBitParams

writeWriteBitPolicyScriptsV3 :: IO ()
writeWriteBitPolicyScriptsV3 = do
  writeSuceedingV3Script
    "succeedingWriteBitsPolicyScriptV3"
    $$(PlutusTx.compile [|| mkWriteBitsPolicy ||])
    succeedingWriteBitsParams
  writeFailingV3Scripts
    "failingWriteBitPolicyScriptV3"
    $$(PlutusTx.compile [|| mkWriteBitsPolicy ||])
    failingWriteBitsParams

writeReplicateBytePolicyScriptsV3 :: IO ()
writeReplicateBytePolicyScriptsV3 = do
  writeSuceedingV3Script
    "succeedingReplicateBytePolicyScriptV3"
    $$(PlutusTx.compile [|| mkReplicateBytePolicy ||])
    succeedingReplicateByteParams
  writeFailingV3Scripts
    "failingReplicateBytePolicyScriptV3"
    $$(PlutusTx.compile [|| mkReplicateBytePolicy ||])
    failingReplicateByteParams


writeReplicateByteStringPolicyScriptsV3 :: IO ()
writeReplicateByteStringPolicyScriptsV3 = pure()

writeBitwisePolicyScriptsV3 :: IO ()
writeBitwisePolicyScriptsV3 = do
  writeAndByteStringPolicyScriptsV3
  writeOrByteStringPolicyScriptsV3
  writeXorByteStringPolicyScriptsV3
  writeComplementByteStringPolicyScriptsV3
  writeShiftByteStringPolicyScriptsV3
  writeRotateByteStringPolicyScriptsV3
  writeCountSetBitsPolicyScriptsV3
  writeFindFirstSetBitPolicyScriptsV3
  writeReadBitPolicyScriptsV3
  writeWriteBitPolicyScriptsV3
  writeReplicateBytePolicyScriptsV3
