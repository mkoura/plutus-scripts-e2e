{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Bitwise.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))
import PlutusLedgerApi.V3 qualified as PlutusV3
import PlutusScripts.Bitwise.Complement (
  mkComplementByteStringPolicy,
  succeedingComplementByteStringParams,
 )
import PlutusScripts.Bitwise.CountFindFirstSet (
  mkCountSetBitsPolicy,
  mkFindFirstSetBitPolicy,
  succeedingCountSetBitsParams,
  succeedingFindFirstSetBitParams,
 )
import PlutusScripts.Bitwise.Conversions (
  mkByteStringToIntegerPolicy,
  mkByteStringToIntegerRoundtripPolicy,
  mkIntegerToByteStringPolicy,
 )
import PlutusScripts.Bitwise.Logical (
  mkAndByteStringPolicy,
  mkOrByteStringPolicy,
  mkXorByteStringPolicy,
  succeedingAndByteStringParams,
  succeedingOrByteStringParams,
  succeedingXorByteStringParams,
 )
import PlutusScripts.Bitwise.ReadBit (
  failingReadBitParams,
  mkReadBitPolicy,
  succeedingReadBitParams,
 )
import PlutusScripts.Bitwise.ReplicateByte (
  failingReplicateByteParams,
  mkReplicateBytePolicy,
  succeedingReplicateByteParams,
 )
import PlutusScripts.Bitwise.ShiftRotate (
  mkRotateByteStringPolicy,
  mkShiftByteStringPolicy,
  succeedingRotateByteStringParams,
  succeedingShiftByteStringParams,
 )
import PlutusScripts.Bitwise.WriteBits (
  failingWriteBitsParams,
  mkWriteBitsPolicy,
  succeedingWriteBitsParams,
 )
import PlutusScripts.Helpers (writeCompiledScript)
import PlutusTx qualified
import PlutusTx.Code (CompiledCodeIn)

-- ByteString to Integer --

byteStringToIntegerPolicyV3 :: SerialisedScript
byteStringToIntegerPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkByteStringToIntegerPolicy

-- Integer to ByteString --

integerToByteStringPolicyV3 :: SerialisedScript
integerToByteStringPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkIntegerToByteStringPolicy

-- ByteString to Integer and Integer to ByteString Roundtrip --

byteStringToIntegerRoundtripPolicyV3 :: SerialisedScript
byteStringToIntegerRoundtripPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkByteStringToIntegerRoundtripPolicy

{- Simple end-to-end tests for bitwise builtins in PlutusV3.  All of these are
   self-contained: the inputs are compiled into the script rather than being
   obtained from a redeemer or similar. -}

-- Each script takes a list of inputs and expected results and iterates over
-- them, checking that the given inputs produce the expected result.
writeSuceedingV3Script ::
  forall param r.
  ( PlutusTx.Lift DefaultUni [param]
  , PlutusTx.Typeable DefaultUni [param]
  , PlutusTx.Typeable DefaultUni param
  ) =>
  String ->
  (CompiledCodeIn DefaultUni DefaultFun ([param] -> r)) ->
  [param] ->
  IO ()
writeSuceedingV3Script name code params =
  let compiledCode =
        code
          `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 params)
   in writeCompiledScript PlutusV3 name compiledCode

-- This takes a list of inputs which are expected to cause a failure.  For
-- failing tests we have to produce a separate script for every set of inputs
-- because we want to check that *all* cases fail.  We re-use the same script as
-- for succeeding inputs but supply it with a list containing a single set of
-- inputs.
writeFailingV3Scripts ::
  forall param r.
  ( PlutusTx.Lift DefaultUni [param]
  , PlutusTx.Typeable DefaultUni [param]
  , PlutusTx.Typeable DefaultUni param
  ) =>
  String ->
  (CompiledCodeIn DefaultUni DefaultFun ([param] -> r)) ->
  [param] ->
  IO ()
writeFailingV3Scripts name code params =
  let writeOneScript (n :: Integer, param) =
        let compiledCode =
              code
                `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 [param])
         in writeCompiledScript PlutusV3 (name ++ "_" ++ show n) compiledCode
   in mapM_ writeOneScript $ zip [1 ..] params

writeAndByteStringPolicyScriptsV3 :: IO ()
writeAndByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingAndByteStringPolicyScriptV3"
    $$(PlutusTx.compile [||mkAndByteStringPolicy||])
    succeedingAndByteStringParams

writeOrByteStringPolicyScriptsV3 :: IO ()
writeOrByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingOrByteStringPolicyScriptV3"
    $$(PlutusTx.compile [||mkOrByteStringPolicy||])
    succeedingOrByteStringParams

writeXorByteStringPolicyScriptsV3 :: IO ()
writeXorByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingXorByteStringPolicyScriptV3"
    $$(PlutusTx.compile [||mkXorByteStringPolicy||])
    succeedingXorByteStringParams

writeComplementByteStringPolicyScriptsV3 :: IO ()
writeComplementByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingComplementByteStringPolicyScriptV3"
    $$(PlutusTx.compile [||mkComplementByteStringPolicy||])
    succeedingComplementByteStringParams

writeShiftByteStringPolicyScriptsV3 :: IO ()
writeShiftByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingShiftByteStringPolicyScriptV3"
    $$(PlutusTx.compile [||mkShiftByteStringPolicy||])
    succeedingShiftByteStringParams

writeRotateByteStringPolicyScriptsV3 :: IO ()
writeRotateByteStringPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingRotateByteStringPolicyScriptV3"
    $$(PlutusTx.compile [||mkRotateByteStringPolicy||])
    succeedingRotateByteStringParams

writeCountSetBitsPolicyScriptsV3 :: IO ()
writeCountSetBitsPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingCountSetBitsPolicyScriptV3"
    $$(PlutusTx.compile [||mkCountSetBitsPolicy||])
    succeedingCountSetBitsParams

writeFindFirstSetBitPolicyScriptsV3 :: IO ()
writeFindFirstSetBitPolicyScriptsV3 =
  writeSuceedingV3Script
    "succeedingFindFirstSetBitPolicyScriptV3"
    $$(PlutusTx.compile [||mkFindFirstSetBitPolicy||])
    succeedingFindFirstSetBitParams

writeReadBitPolicyScriptsV3 :: IO ()
writeReadBitPolicyScriptsV3 = do
  writeSuceedingV3Script
    "succeedingReadBitPolicyScriptV3"
    $$(PlutusTx.compile [||mkReadBitPolicy||])
    succeedingReadBitParams
  writeFailingV3Scripts
    "failingReadBitPolicyScriptV3"
    $$(PlutusTx.compile [||mkReadBitPolicy||])
    failingReadBitParams

writeWriteBitPolicyScriptsV3 :: IO ()
writeWriteBitPolicyScriptsV3 = do
  writeSuceedingV3Script
    "succeedingWriteBitsPolicyScriptV3"
    $$(PlutusTx.compile [||mkWriteBitsPolicy||])
    succeedingWriteBitsParams
  writeFailingV3Scripts
    "failingWriteBitsPolicyScriptV3"
    $$(PlutusTx.compile [||mkWriteBitsPolicy||])
    failingWriteBitsParams

writeReplicateBytePolicyScriptsV3 :: IO ()
writeReplicateBytePolicyScriptsV3 = do
  writeSuceedingV3Script
    "succeedingReplicateBytePolicyScriptV3"
    $$(PlutusTx.compile [||mkReplicateBytePolicy||])
    succeedingReplicateByteParams
  writeFailingV3Scripts
    "failingReplicateBytePolicyScriptV3"
    $$(PlutusTx.compile [||mkReplicateBytePolicy||])
    failingReplicateByteParams

writeReplicateByteStringPolicyScriptsV3 :: IO ()
writeReplicateByteStringPolicyScriptsV3 = pure ()

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
