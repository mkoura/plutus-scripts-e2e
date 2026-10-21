{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-version=1.1.0 #-}

-- | PlutusV3 / PLC 1.1.0 policies for multiIndexArray (batch 7).
module PlutusScripts.Batch7.MultiIndexArray.V3_110 (multiIndexArrayScripts_V3_110) where

import Helpers.Envelopes (VersionedScript (VersionedScript), plc110)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))
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

-- | Named policies; each failing policy isolates one observable failure.
multiIndexArrayScripts_V3_110 :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
multiIndexArrayScripts_V3_110 =
  map (uncurry (VersionedScript PlutusV3 plc110)) $
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
      `unsafeApplyCode` liftCode plcVersion110 count
  failing :: [Integer] -> [Integer] -> CompiledCode (P.BuiltinData -> P.BuiltinUnit)
  failing elements indices =
    $$(compile [||mkFailingPolicy||])
      `unsafeApplyCode` liftCode plcVersion110 elements
      `unsafeApplyCode` liftCode plcVersion110 indices
