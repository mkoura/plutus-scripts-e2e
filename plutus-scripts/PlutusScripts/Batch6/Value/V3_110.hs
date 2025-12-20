{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Batch6.Value.V3_110 where

import PlutusScripts.Batch6.Value.Common (
  mkInsertNewCoinPolicy,
  mkInsertExistingCoinPolicy,
  mkDeleteExistingCoinPolicy,
  mkDeleteMissingCoinPolicy,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

succeedingInsertNewCoinPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingInsertNewCoinPolicy =
  $$(PlutusTx.compile [||mkInsertNewCoinPolicy||])

succeedingInsertExistingCoinPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingInsertExistingCoinPolicy =
  $$(PlutusTx.compile [||mkInsertExistingCoinPolicy||])

succeedingDeleteExistingCoinPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingDeleteExistingCoinPolicy =
  $$(PlutusTx.compile [||mkDeleteExistingCoinPolicy||])

succeedingDeleteMissingCoinPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingDeleteMissingCoinPolicy =
  $$(PlutusTx.compile [||mkDeleteMissingCoinPolicy||])
