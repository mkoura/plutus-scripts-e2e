{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Hashing.V_1_1 where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Hashing.Common (hashingAssetName, hashingParamsV3, mkHashingPolicyV3)
import PlutusScripts.Hashing.Ripemd_160 (mkRipemd_160Policy, succeedingRipemd_160Params)
import PlutusScripts.Helpers (
  mintScriptWitness,
  plutusL3,
  policyIdV3,
  toScriptData,
  writeSerialisedScript
 )
import PlutusTx qualified

-- checkHashingPolicyV3 :: SerialisedScript
-- checkHashingPolicyV3 =
--   serialiseCompiledCode
--     $$(PlutusTx.compile [||wrap||])mkHashingPolicy
--   where
--     wrap = mkUntypedMintingPolicy @PlutusV3.ScriptContext mkHashingPolicy

checkHashingPolicy :: SerialisedScript
checkHashingPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkHashingPolicyV3||])
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 hashingParamsV3)

checkHashingPolicyScriptV3 :: C.PlutusScript C.PlutusScriptV3
checkHashingPolicyScriptV3 = C.PlutusScriptSerialised checkHashingPolicy

checkHashingAssetIdV3 :: C.AssetId
checkHashingAssetIdV3 = C.AssetId (policyIdV3 checkHashingPolicy) hashingAssetName

checkHashingMintWitnessV3
  :: C.ShelleyBasedEra era
  -> (C.PolicyId, C.ScriptWitness C.WitCtxMint era)
checkHashingMintWitnessV3 sbe =
  ( policyIdV3 checkHashingPolicy
  , mintScriptWitness sbe plutusL3 (Left checkHashingPolicyScriptV3) (toScriptData ())
  )

-- A separate test for ripemd_160 since that only appeared in PlutusV3

-- Each script takes a list of inputs and expected results and iterates over
-- them, checking that the given inputs produce the expected result.
-- This is a duplicate of a function in the bitwise tests.
writeSuceedingV3Script
  :: PlutusTx.Lift DefaultUni [param]
  => String
  -> (PlutusTx.CompiledCodeIn DefaultUni DefaultFun ([param] -> r))
  -> [param]
  -> IO ()
writeSuceedingV3Script name code params =
  let script :: C.PlutusScript C.PlutusScriptV3
      script = C.PlutusScriptSerialised $ serialiseCompiledCode (code `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 params))
  in writeSerialisedScript name script

writeRipemd_160PolicyScriptV3 :: IO ()
writeRipemd_160PolicyScriptV3 =
  writeSuceedingV3Script
    "succeedingRipemd_160Policy"
    $$(PlutusTx.compile [|| mkRipemd_160Policy ||])
    succeedingRipemd_160Params

