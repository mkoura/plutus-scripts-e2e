{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Hashing.V_1_1 where

import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))
import PlutusScripts.Hashing.Common (hashingParamsV3, mkHashingPolicyV3)
import PlutusScripts.Hashing.Ripemd_160 (mkRipemd_160Policy, succeedingRipemd_160Params)
import PlutusScripts.Helpers (writeCompiledScript)
import PlutusTx qualified
import PlutusTx.Code (CompiledCodeIn)

checkHashingPolicy :: SerialisedScript
checkHashingPolicy =
  serialiseCompiledCode $
    $$(PlutusTx.compile [||mkHashingPolicyV3||])
      `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 hashingParamsV3)

-- A separate test for ripemd_160 since that only appeared in PlutusV3

-- Each script takes a list of inputs and expected results and iterates over
-- them, checking that the given inputs produce the expected result.
-- This is a duplicate of a function in the bitwise tests.
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

writeRipemd_160PolicyScriptV3 :: IO ()
writeRipemd_160PolicyScriptV3 =
  writeSuceedingV3Script
    "succeedingRipemd_160Policy"
    $$(PlutusTx.compile [||mkRipemd_160Policy||])
    succeedingRipemd_160Params
