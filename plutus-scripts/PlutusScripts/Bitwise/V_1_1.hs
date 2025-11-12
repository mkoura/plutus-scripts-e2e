{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Bitwise.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.Bitwise.Complement (
  mkComplementByteStringPolicy,
  succeedingComplementByteStringParams,
 )
import PlutusScripts.Bitwise.Conversions (
  mkByteStringToIntegerPolicy,
  mkByteStringToIntegerRoundtripPolicy,
  mkIntegerToByteStringPolicy,
 )
import PlutusScripts.Bitwise.CountFindFirstSet (
  mkCountSetBitsPolicy,
  mkFindFirstSetBitPolicy,
  succeedingCountSetBitsParams,
  succeedingFindFirstSetBitParams,
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
  mkReadBitPolicy,
  succeedingReadBitParams,
 )
import PlutusScripts.Bitwise.ReplicateByte (
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
  mkWriteBitsPolicy,
  succeedingWriteBitsParams,
 )
import PlutusTx qualified
import PlutusTx.Code (CompiledCodeIn)
import PlutusTx.Prelude qualified as P

-- ByteString to Integer --

byteStringToIntegerPolicyV3 :: SerialisedScript
byteStringToIntegerPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
 where
  wrap = mkUntypedMintingPolicy @V3.ScriptContext mkByteStringToIntegerPolicy

-- Integer to ByteString --

integerToByteStringPolicyV3 :: SerialisedScript
integerToByteStringPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
 where
  wrap = mkUntypedMintingPolicy @V3.ScriptContext mkIntegerToByteStringPolicy

-- ByteString to Integer and Integer to ByteString Roundtrip --

byteStringToIntegerRoundtripPolicyV3 :: SerialisedScript
byteStringToIntegerRoundtripPolicyV3 =
  serialiseCompiledCode
    $$(PlutusTx.compile [||wrap||])
 where
  wrap = mkUntypedMintingPolicy @V3.ScriptContext mkByteStringToIntegerRoundtripPolicy

{- Simple end-to-end tests for bitwise builtins in V3.  All of these are
   self-contained: the inputs are compiled into the script rather than being
   obtained from a redeemer or similar. -}

-- Compiled code values with parameters already applied for succeeding tests
succeedingAndByteStringPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingAndByteStringPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkAndByteStringPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 succeedingAndByteStringParams

succeedingOrByteStringPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingOrByteStringPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkOrByteStringPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 succeedingOrByteStringParams

succeedingXorByteStringPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingXorByteStringPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkXorByteStringPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 succeedingXorByteStringParams

succeedingComplementByteStringPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingComplementByteStringPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkComplementByteStringPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 succeedingComplementByteStringParams

succeedingShiftByteStringPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingShiftByteStringPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkShiftByteStringPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 succeedingShiftByteStringParams

succeedingRotateByteStringPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingRotateByteStringPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkRotateByteStringPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 succeedingRotateByteStringParams

succeedingCountSetBitsPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingCountSetBitsPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkCountSetBitsPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 succeedingCountSetBitsParams

succeedingFindFirstSetBitPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingFindFirstSetBitPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkFindFirstSetBitPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 succeedingFindFirstSetBitParams

succeedingReadBitPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingReadBitPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkReadBitPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 succeedingReadBitParams

succeedingWriteBitsPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingWriteBitsPolicyCompiledV3 =
  $$(PlutusTx.compile [||mkWriteBitsPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 succeedingWriteBitsParams

succeedingReplicateBytePolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingReplicateBytePolicyCompiledV3 =
  $$(PlutusTx.compile [||mkReplicateBytePolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion110 succeedingReplicateByteParams

-- Note: Failing test scripts are not currently generated.
-- They require applying individual failing params which would need the Params types to be exported.
-- For now, only succeeding tests are supported.
