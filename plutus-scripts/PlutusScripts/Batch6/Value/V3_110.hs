{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Batch6.Value.V3_110 where

import PlutusScripts.Batch6.Value.Common (
  mkInsertNewCoinPolicy,
  mkInsertExistingCoinPolicy,
  mkDeleteExistingCoinPolicy,
  mkDeleteMissingCoinPolicy,
  mkInsertInvalidCurrencySymbolPolicy,
  mkInsertInvalidTokenNamePolicy,
  mkInsertOverflowQuantityPolicy,
  mkInsertUnderflowQuantityPolicy,
  mkLookupMissingCoinPolicy,
  mkValueContainsEmptyPolicy,
  mkValueContainsLeftNegativePolicy,
  mkValueContainsRightNegativePolicy,
  mkValueContainsReflexivePolicy,
  mkValueContainsDisjointPolicy,
  mkValueContainsRightExtraKeyPolicy,
  mkValueContainsRightHigherAmountPolicy,
  mkValueContainsIsSubValuePolicy,
  mkValueContainsIsSubValueSmallerAmountPolicy,
  mkUnionValueEmptyIdentityPolicy,
  mkUnionValueAssociativePolicy,
  mkUnionValueAssociativeSingleCoinPolicy,
  mkUnionValueCommutativePolicy,
  mkUnionValueCommutativeSingleCoinPolicy,
  mkUnionValueInversablePolicy,
  mkUnionValueOverflowPolicy,
  mkUnionValueUnderflowPolicy,
  mkScaleValueZeroPolicy,
  mkScaleValuePositivePolicy,
  mkScaleValueNegativePolicy,
  mkScaleValueOverflowPolicy,
  mkScaleValueUnderflowPolicy,
  mkValueDataRoundTripPolicy,
  mkUnValueDataInvalidDataPolicy,
 )
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

succeedingLookupMissingCoinPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingLookupMissingCoinPolicy =
  $$(PlutusTx.compile [||mkLookupMissingCoinPolicy||])

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

failingInsertInvalidCurrencySymbolPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
failingInsertInvalidCurrencySymbolPolicy =
  $$(PlutusTx.compile [||mkInsertInvalidCurrencySymbolPolicy||])

failingInsertInvalidTokenNamePolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
failingInsertInvalidTokenNamePolicy =
  $$(PlutusTx.compile [||mkInsertInvalidTokenNamePolicy||])

failingInsertOverflowQuantityPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
failingInsertOverflowQuantityPolicy =
  $$(PlutusTx.compile [||mkInsertOverflowQuantityPolicy||])

failingInsertUnderflowQuantityPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
failingInsertUnderflowQuantityPolicy =
  $$(PlutusTx.compile [||mkInsertUnderflowQuantityPolicy||])

succeedingValueContainsEmptyPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingValueContainsEmptyPolicy =
  $$(PlutusTx.compile [||mkValueContainsEmptyPolicy||])

failingValueContainsLeftNegativePolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
failingValueContainsLeftNegativePolicy =
  $$(PlutusTx.compile [||mkValueContainsLeftNegativePolicy||])

failingValueContainsRightNegativePolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
failingValueContainsRightNegativePolicy =
  $$(PlutusTx.compile [||mkValueContainsRightNegativePolicy||])

succeedingValueContainsReflexivePolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingValueContainsReflexivePolicy =
  $$(PlutusTx.compile [||mkValueContainsReflexivePolicy||])

succeedingValueContainsDisjointPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingValueContainsDisjointPolicy =
  $$(PlutusTx.compile [||mkValueContainsDisjointPolicy||])

succeedingValueContainsRightExtraKeyPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingValueContainsRightExtraKeyPolicy =
  $$(PlutusTx.compile [||mkValueContainsRightExtraKeyPolicy||])

succeedingValueContainsRightHigherAmountPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingValueContainsRightHigherAmountPolicy =
  $$(PlutusTx.compile [||mkValueContainsRightHigherAmountPolicy||])

succeedingValueContainsIsSubValuePolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingValueContainsIsSubValuePolicy =
  $$(PlutusTx.compile [||mkValueContainsIsSubValuePolicy||])

succeedingValueContainsIsSubValueSmallerAmountPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingValueContainsIsSubValueSmallerAmountPolicy =
  $$(PlutusTx.compile [||mkValueContainsIsSubValueSmallerAmountPolicy||])

succeedingUnionValueEmptyIdentityPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingUnionValueEmptyIdentityPolicy =
  $$(PlutusTx.compile [||mkUnionValueEmptyIdentityPolicy||])

succeedingUnionValueAssociativePolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingUnionValueAssociativePolicy =
  $$(PlutusTx.compile [||mkUnionValueAssociativePolicy||])

succeedingUnionValueAssociativeSingleCoinPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingUnionValueAssociativeSingleCoinPolicy =
  $$(PlutusTx.compile [||mkUnionValueAssociativeSingleCoinPolicy||])

succeedingUnionValueCommutativePolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingUnionValueCommutativePolicy =
  $$(PlutusTx.compile [||mkUnionValueCommutativePolicy||])

succeedingUnionValueCommutativeSingleCoinPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingUnionValueCommutativeSingleCoinPolicy =
  $$(PlutusTx.compile [||mkUnionValueCommutativeSingleCoinPolicy||])

succeedingUnionValueInversablePolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingUnionValueInversablePolicy =
  $$(PlutusTx.compile [||mkUnionValueInversablePolicy||])

failingUnionValueOverflowPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
failingUnionValueOverflowPolicy =
  $$(PlutusTx.compile [||mkUnionValueOverflowPolicy||])

failingUnionValueUnderflowPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
failingUnionValueUnderflowPolicy =
  $$(PlutusTx.compile [||mkUnionValueUnderflowPolicy||])

succeedingScaleValueZeroPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingScaleValueZeroPolicy =
  $$(PlutusTx.compile [||mkScaleValueZeroPolicy||])

succeedingScaleValuePositivePolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingScaleValuePositivePolicy =
  $$(PlutusTx.compile [||mkScaleValuePositivePolicy||])

succeedingScaleValueNegativePolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingScaleValueNegativePolicy =
  $$(PlutusTx.compile [||mkScaleValueNegativePolicy||])

failingScaleValueOverflowPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
failingScaleValueOverflowPolicy =
  $$(PlutusTx.compile [||mkScaleValueOverflowPolicy||])

failingScaleValueUnderflowPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
failingScaleValueUnderflowPolicy =
  $$(PlutusTx.compile [||mkScaleValueUnderflowPolicy||])

succeedingValueDataRoundTripPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
succeedingValueDataRoundTripPolicy =
  $$(PlutusTx.compile [||mkValueDataRoundTripPolicy||])

failingUnValueDataInvalidDataPolicy
  :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> P.BuiltinUnit)
failingUnValueDataInvalidDataPolicy =
  $$(PlutusTx.compile [||mkUnValueDataInvalidDataPolicy||])