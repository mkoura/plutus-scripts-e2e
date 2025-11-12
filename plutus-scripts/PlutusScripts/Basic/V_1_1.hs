{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Basic.V_1_1 where

import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.Basic.Common (
  mkAlwaysFailsPolicyV3,
  mkAlwaysSucceedPolicyV3,
  mkAlwaysSucceedSpend,
  mkMintTokenNamePolicyV3,
  mkTimeRangePolicyV3,
  mkWitnessRedeemerPolicyV3,
 )
import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude qualified as P

-- AlwaysSucceeds minting policy --

alwaysSucceedPolicyCompiled :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
alwaysSucceedPolicyCompiled = $$(compile [||mkAlwaysSucceedPolicyV3||])

alwaysSucceedPolicy :: V3.SerialisedScript
alwaysSucceedPolicy = V3.serialiseCompiledCode alwaysSucceedPolicyCompiled

-- AlwaysSucceeds validator --

alwaysSucceedSpend :: V3.SerialisedScript
alwaysSucceedSpend = V3.serialiseCompiledCode $$(compile [||mkAlwaysSucceedSpend||])

-- AlwaysFails minting policy --

alwaysFailsPolicyCompiled :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
alwaysFailsPolicyCompiled = $$(compile [||mkAlwaysFailsPolicyV3||])

alwaysFailsPolicy :: V3.SerialisedScript
alwaysFailsPolicy = V3.serialiseCompiledCode alwaysFailsPolicyCompiled

-- Mint token name policy --

mintTokenNamePolicyCompiledV3 :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
mintTokenNamePolicyCompiledV3 = $$(compile [||mkMintTokenNamePolicyV3||])

mintTokenNamePolicyV3 :: V3.SerialisedScript
mintTokenNamePolicyV3 = V3.serialiseCompiledCode mintTokenNamePolicyCompiledV3

-- Time range policy --

timeRangePolicyCompiledV3 :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
timeRangePolicyCompiledV3 = $$(compile [||mkTimeRangePolicyV3||])

timeRangePolicyV3 :: V3.SerialisedScript
timeRangePolicyV3 = V3.serialiseCompiledCode timeRangePolicyCompiledV3

-- Witness redeemer policy --

witnessRedeemerPolicyCompiledV3
  :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
witnessRedeemerPolicyCompiledV3 = $$(compile [||mkWitnessRedeemerPolicyV3||])

witnessRedeemerPolicyV3 :: V3.SerialisedScript
witnessRedeemerPolicyV3 = V3.serialiseCompiledCode witnessRedeemerPolicyCompiledV3
