{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
-- preserves traces
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module PlutusScripts.Basic.V_1_1 where

import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))
import PlutusLedgerApi.V3 qualified as PlutusV3 (serialiseCompiledCode)
import PlutusScripts.Basic.Common (
  mkAlwaysFailsPolicyV3,
  mkAlwaysSucceedPolicyV3,
  mkAlwaysSucceedSpend,
  mkMintTokenNamePolicyV3,
  mkTimeRangePolicyV3,
  mkWitnessRedeemerPolicyV3,
 )
import PlutusScripts.Helpers (writeCompiledScript)
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

-- AlwaysSucceeds minting policy --

alwaysSucceedPolicyCompiled :: PlutusTx.CompiledCode (P.BuiltinData -> P.BuiltinUnit)
alwaysSucceedPolicyCompiled = $$(PlutusTx.compile [||mkAlwaysSucceedPolicyV3||])

alwaysSucceedPolicy :: SerialisedScript
alwaysSucceedPolicy = serialiseCompiledCode alwaysSucceedPolicyCompiled

writeAlwaysSucceedPolicyScriptV3 :: IO ()
writeAlwaysSucceedPolicyScriptV3 =
  writeCompiledScript PlutusV3 "alwaysSucceedPolicyScriptV3" alwaysSucceedPolicyCompiled

-- AlwaysSucceeds validator --

alwaysSucceedSpend :: SerialisedScript
alwaysSucceedSpend = serialiseCompiledCode $$(PlutusTx.compile [||mkAlwaysSucceedSpend||])

-- AlwaysFails minting policy --

alwaysFailsPolicyCompiled :: PlutusTx.CompiledCode (P.BuiltinData -> P.BuiltinUnit)
alwaysFailsPolicyCompiled = $$(PlutusTx.compile [||mkAlwaysFailsPolicyV3||])

alwaysFailsPolicy :: SerialisedScript
alwaysFailsPolicy = serialiseCompiledCode alwaysFailsPolicyCompiled

writeAlwaysFailsPolicyScriptV3 :: IO ()
writeAlwaysFailsPolicyScriptV3 =
  writeCompiledScript PlutusV3 "alwaysFailsPolicyScriptV3" alwaysFailsPolicyCompiled

-- Mint token name policy --

mintTokenNamePolicyCompiledV3 :: PlutusTx.CompiledCode (P.BuiltinData -> P.BuiltinUnit)
mintTokenNamePolicyCompiledV3 = $$(PlutusTx.compile [||mkMintTokenNamePolicyV3||])

mintTokenNamePolicyV3 :: SerialisedScript
mintTokenNamePolicyV3 = PlutusV3.serialiseCompiledCode mintTokenNamePolicyCompiledV3

writeTokenNamePolicyScriptV3 :: IO ()
writeTokenNamePolicyScriptV3 =
  writeCompiledScript PlutusV3 "mintTokenNamePolicyScriptV3" mintTokenNamePolicyCompiledV3

-- Time range policy --

timeRangePolicyCompiledV3 :: PlutusTx.CompiledCode (P.BuiltinData -> P.BuiltinUnit)
timeRangePolicyCompiledV3 = $$(PlutusTx.compile [||mkTimeRangePolicyV3||])

timeRangePolicyV3 :: SerialisedScript
timeRangePolicyV3 = PlutusV3.serialiseCompiledCode timeRangePolicyCompiledV3

writeTimeRangePolicyScriptV3 :: IO ()
writeTimeRangePolicyScriptV3 =
  writeCompiledScript PlutusV3 "timeRangePolicyScriptV3" timeRangePolicyCompiledV3

-- Witness redeemer policy --

witnessRedeemerPolicyCompiledV3
  :: PlutusTx.CompiledCode (P.BuiltinData -> P.BuiltinUnit)
witnessRedeemerPolicyCompiledV3 = $$(PlutusTx.compile [||mkWitnessRedeemerPolicyV3||])

witnessRedeemerPolicyV3 :: SerialisedScript
witnessRedeemerPolicyV3 = PlutusV3.serialiseCompiledCode witnessRedeemerPolicyCompiledV3

writeWitnessRedeemerPolicyScriptV3 :: IO ()
writeWitnessRedeemerPolicyScriptV3 =
  writeCompiledScript
    PlutusV3
    "witnessRedeemerPolicyScriptV3"
    witnessRedeemerPolicyCompiledV3
