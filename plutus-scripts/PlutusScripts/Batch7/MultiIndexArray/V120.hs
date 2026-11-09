{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-version=1.2.0 #-}

-- | PlutusV3 and PlutusV4 / PLC 1.2.0 multiIndexArray policies.
module PlutusScripts.Batch7.MultiIndexArray.V120 (
  allMultiIndexArrayScripts,
) where

import Data.Text (Text)
import Helpers.Envelopes (VersionedScript (VersionedScript), plc120)
import PlutusCore.Version (plcVersion120)
import PlutusLedgerApi.Common.Versions (
  PlutusLedgerLanguage (PlutusV3, PlutusV4),
 )
import PlutusScripts.Batch7.MultiIndexArray.Common (
  failingLookups,
  mkDataPolicy,
  mkFailingPolicy,
  mkIntegerPolicy,
  mkOverLimitPolicy,
  mkRepeatedPolicy,
  repeatedSelections,
 )
import PlutusTx (CompiledCode, compile, liftCode, unsafeApplyCode)
import PlutusTx.Prelude qualified as P

-- | Context-independent policies share compiled code across ledger languages.
allMultiIndexArrayScripts :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
allMultiIndexArrayScripts = concatMap multiIndexArrayScripts [PlutusV3, PlutusV4]

multiIndexArrayScripts
  :: PlutusLedgerLanguage
  -> [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
multiIndexArrayScripts ll =
  map (uncurry (VersionedScript ll plc120)) policies

-- | Reuse the PLC 1.1.0 scenarios, compiling and lifting at PLC 1.2.0.
policies :: [(Text, CompiledCode (P.BuiltinData -> P.BuiltinUnit))]
policies =
  [ ("succeedingMultiIndexArrayPolicyScript", $$(compile [||mkIntegerPolicy||]))
  , ("succeedingMultiIndexArrayDataPolicyScript", $$(compile [||mkDataPolicy||]))
  ]
    <> [(name, repeated count) | (name, count) <- repeatedSelections]
    <> [(name, failing elements indices) | (name, elements, indices) <- failingLookups]
    <> [("failingMultiIndexArrayOverLimitPolicyScript", $$(compile [||mkOverLimitPolicy||]))]
 where
  repeated :: Integer -> CompiledCode (P.BuiltinData -> P.BuiltinUnit)
  repeated count =
    $$(compile [||mkRepeatedPolicy||])
      `unsafeApplyCode` liftCode plcVersion120 count
  failing
    :: [Integer]
    -> [Integer]
    -> CompiledCode (P.BuiltinData -> P.BuiltinUnit)
  failing elements indices =
    $$(compile [||mkFailingPolicy||])
      `unsafeApplyCode` liftCode plcVersion120 elements
      `unsafeApplyCode` liftCode plcVersion120 indices
