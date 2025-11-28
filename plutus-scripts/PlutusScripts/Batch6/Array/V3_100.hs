{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Batch6.Array.V3_100 where

import PlutusScripts.Batch6.Array.Common (
  mkIndexArrayPolicy,
  mkLengthOfArrayPolicy,
  mkListToArrayPolicy,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

succeedingIndexArrayPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingIndexArrayPolicy =
  $$(PlutusTx.compile [||mkIndexArrayPolicy||])

succeedingLengthOfArrayPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingLengthOfArrayPolicy =
  $$(PlutusTx.compile [||mkLengthOfArrayPolicy||])

succeedingListToArrayPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingListToArrayPolicy =
  $$(PlutusTx.compile [||mkListToArrayPolicy||])
