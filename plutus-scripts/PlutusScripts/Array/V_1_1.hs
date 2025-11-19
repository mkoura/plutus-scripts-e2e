{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Array.V_1_1 where

import PlutusScripts.Array.Common (
  mkIndexArrayPolicy,
  mkLengthOfArrayPolicy,
  mkListToArrayPolicy,
 )
import PlutusTx (compile)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as P

--------------------------------------------------------------------------------
-- Succeeding Array Tests (PlutusV3) -------------------------------------------

succeedingIndexArrayPolicyCompiledV3 :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingIndexArrayPolicyCompiledV3 = $$(compile [||mkIndexArrayPolicy||])

succeedingLengthOfArrayPolicyCompiledV3 :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingLengthOfArrayPolicyCompiledV3 = $$(compile [||mkLengthOfArrayPolicy||])

succeedingListToArrayPolicyCompiledV3 :: CompiledCode (P.BuiltinData -> P.BuiltinUnit)
succeedingListToArrayPolicyCompiledV3 = $$(compile [||mkListToArrayPolicy||])
