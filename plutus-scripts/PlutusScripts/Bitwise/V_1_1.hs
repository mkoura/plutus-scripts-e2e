{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Bitwise.V_1_1 where

import Helpers.ScriptUtils (
  IsScriptContext (mkUntypedMintingPolicy),
  ScriptGroup (ScriptGroup, sgBaseName, sgScripts),
 )
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion110)
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
import PlutusTx (compile, liftCode, unsafeApplyCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as P

-- ByteString to Integer --

byteStringToIntegerPolicyV3 :: V3.SerialisedScript
byteStringToIntegerPolicyV3 =
  V3.serialiseCompiledCode
    $$(compile [||wrap||])
 where
  wrap = mkUntypedMintingPolicy @V3.ScriptContext mkByteStringToIntegerPolicy

-- Integer to ByteString --

integerToByteStringPolicyV3 :: V3.SerialisedScript
integerToByteStringPolicyV3 =
  V3.serialiseCompiledCode
    $$(compile [||wrap||])
 where
  wrap = mkUntypedMintingPolicy @V3.ScriptContext mkIntegerToByteStringPolicy

-- ByteString to Integer and Integer to ByteString Roundtrip --

byteStringToIntegerRoundtripPolicyV3 :: V3.SerialisedScript
byteStringToIntegerRoundtripPolicyV3 =
  V3.serialiseCompiledCode
    $$(compile [||wrap||])
 where
  wrap = mkUntypedMintingPolicy @V3.ScriptContext mkByteStringToIntegerRoundtripPolicy

{- Simple end-to-end tests for bitwise builtins in V3.  All of these are
   self-contained: the inputs are compiled into the script rather than being
   obtained from a redeemer or similar. -}

-- Compiled code values with parameters already applied for succeeding tests
succeedingAndByteStringPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingAndByteStringPolicyCompiledV3 =
  $$(compile [||mkAndByteStringPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingAndByteStringParams

succeedingOrByteStringPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingOrByteStringPolicyCompiledV3 =
  $$(compile [||mkOrByteStringPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingOrByteStringParams

succeedingXorByteStringPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingXorByteStringPolicyCompiledV3 =
  $$(compile [||mkXorByteStringPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingXorByteStringParams

succeedingComplementByteStringPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingComplementByteStringPolicyCompiledV3 =
  $$(compile [||mkComplementByteStringPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingComplementByteStringParams

succeedingShiftByteStringPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingShiftByteStringPolicyCompiledV3 =
  $$(compile [||mkShiftByteStringPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingShiftByteStringParams

succeedingRotateByteStringPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingRotateByteStringPolicyCompiledV3 =
  $$(compile [||mkRotateByteStringPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingRotateByteStringParams

succeedingCountSetBitsPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingCountSetBitsPolicyCompiledV3 =
  $$(compile [||mkCountSetBitsPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingCountSetBitsParams

succeedingFindFirstSetBitPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingFindFirstSetBitPolicyCompiledV3 =
  $$(compile [||mkFindFirstSetBitPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingFindFirstSetBitParams

succeedingReadBitPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingReadBitPolicyCompiledV3 =
  $$(compile [||mkReadBitPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingReadBitParams

succeedingWriteBitsPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingWriteBitsPolicyCompiledV3 =
  $$(compile [||mkWriteBitsPolicy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingWriteBitsParams

succeedingReplicateBytePolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingReplicateBytePolicyCompiledV3 =
  $$(compile [||mkReplicateBytePolicy||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingReplicateByteParams

--------------------------------------------------------------------------------
-- Failing Tests ---------------------------------------------------------------

{- | All failing bitwise test script groups
Each group generates numbered scripts (e.g., failingReadBit_1.plutus, failingReadBit_2.plutus)
-}
failingBitwiseScriptGroupsV3 :: [ScriptGroup DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)]
failingBitwiseScriptGroupsV3 =
  [ -- ReadBit failing tests
    ScriptGroup
      { sgBaseName = "failingReadBitPolicyScriptV3"
      , sgScripts = map compileReadBit failingReadBitParams
      }
  , -- WriteBits failing tests
    ScriptGroup
      { sgBaseName = "failingWriteBitsPolicyScriptV3"
      , sgScripts = map compileWriteBits failingWriteBitsParams
      }
  , -- ReplicateByte failing tests
    ScriptGroup
      { sgBaseName = "failingReplicateBytePolicyScriptV3"
      , sgScripts = map compileReplicateByte failingReplicateByteParams
      }
  ]
 where
  compileReadBit param =
    $$(compile [||mkReadBitPolicy||])
      `unsafeApplyCode` liftCode plcVersion110 [param]

  compileWriteBits param =
    $$(compile [||mkWriteBitsPolicy||])
      `unsafeApplyCode` liftCode plcVersion110 [param]

  compileReplicateByte param =
    $$(compile [||mkReplicateBytePolicy||])
      `unsafeApplyCode` liftCode plcVersion110 [param]
