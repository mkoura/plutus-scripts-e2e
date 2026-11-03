{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Basic.V4_110 where

import PlutusLedgerApi.V4 qualified as V4
import PlutusScripts.Basic.Common (
  mkAlwaysFailsPolicyV3,
  mkAlwaysSucceedPolicyV3,
  mkMintTokenNamePolicyV4,
  mkTimeRangePolicyV4,
  mkWitnessRedeemerPolicyV4,
 )
import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude qualified as P

-- Context-independent policies can reuse the V3 implementations.
alwaysSucceedPolicyCompiled_V4_110 :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
alwaysSucceedPolicyCompiled_V4_110 = $$(compile [||mkAlwaysSucceedPolicyV3||])

alwaysSucceedPolicy_V4_110 :: V4.SerialisedScript
alwaysSucceedPolicy_V4_110 = V4.serialiseCompiledCode alwaysSucceedPolicyCompiled_V4_110

-- V4 spending validators take only the context, including datum and redeemer.
alwaysSucceedSpendCompiled_V4_110 :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
alwaysSucceedSpendCompiled_V4_110 = $$(compile [||mkAlwaysSucceedPolicyV3||])

alwaysSucceedSpend_V4_110 :: V4.SerialisedScript
alwaysSucceedSpend_V4_110 = V4.serialiseCompiledCode alwaysSucceedSpendCompiled_V4_110

alwaysFailsPolicyCompiled_V4_110 :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
alwaysFailsPolicyCompiled_V4_110 = $$(compile [||mkAlwaysFailsPolicyV3||])

alwaysFailsPolicy_V4_110 :: V4.SerialisedScript
alwaysFailsPolicy_V4_110 = V4.serialiseCompiledCode alwaysFailsPolicyCompiled_V4_110

mintTokenNamePolicyCompiled_V4_110 :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
mintTokenNamePolicyCompiled_V4_110 = $$(compile [||mkMintTokenNamePolicyV4||])

mintTokenNamePolicy_V4_110 :: V4.SerialisedScript
mintTokenNamePolicy_V4_110 = V4.serialiseCompiledCode mintTokenNamePolicyCompiled_V4_110

timeRangePolicyCompiled_V4_110 :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
timeRangePolicyCompiled_V4_110 = $$(compile [||mkTimeRangePolicyV4||])

timeRangePolicy_V4_110 :: V4.SerialisedScript
timeRangePolicy_V4_110 = V4.serialiseCompiledCode timeRangePolicyCompiled_V4_110

witnessRedeemerPolicyCompiled_V4_110 :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
witnessRedeemerPolicyCompiled_V4_110 = $$(compile [||mkWitnessRedeemerPolicyV4||])

witnessRedeemerPolicy_V4_110 :: V4.SerialisedScript
witnessRedeemerPolicy_V4_110 = V4.serialiseCompiledCode witnessRedeemerPolicyCompiled_V4_110
