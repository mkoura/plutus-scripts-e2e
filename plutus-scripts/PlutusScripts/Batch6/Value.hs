module PlutusScripts.Batch6.Value (v3ValueScripts) where

import Helpers.Envelopes (VersionedScript (VersionedScript), plc100, plc110)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))
import PlutusScripts.Batch6.Value.V3_110 qualified as V3_110
import PlutusScripts.Batch6.Value.V3_100 qualified as V3_100
import PlutusTx.Prelude qualified as P

v3ValueScripts :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
v3ValueScripts =
  [ -- V3/1.1.0
    VersionedScript
      PlutusV3
      plc110
      "succeedingLookupMissingCoinPolicyScript"
      V3_110.succeedingLookupMissingCoinPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingInsertNewCoinPolicyScript"
      V3_110.succeedingInsertNewCoinPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingInsertExistingCoinPolicyScript"
      V3_110.succeedingInsertExistingCoinPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingDeleteExistingCoinPolicyScript"
      V3_110.succeedingDeleteExistingCoinPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingDeleteMissingCoinPolicyScript"
      V3_110.succeedingDeleteMissingCoinPolicy
    , VersionedScript
      PlutusV3
      plc110
      "failingInsertInvalidCurrencySymbolPolicyScript"
      V3_110.failingInsertInvalidCurrencySymbolPolicy
    , VersionedScript
      PlutusV3
      plc110
      "failingInsertInvalidTokenNamePolicyScript"
      V3_110.failingInsertInvalidTokenNamePolicy
    , VersionedScript
      PlutusV3
      plc110
      "failingInsertOverflowQuantityPolicyScript"
      V3_110.failingInsertOverflowQuantityPolicy
    , VersionedScript
      PlutusV3
      plc110
      "failingInsertUnderflowQuantityPolicyScript"
      V3_110.failingInsertUnderflowQuantityPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingValueContainsEmptyPolicyScript"
      V3_110.succeedingValueContainsEmptyPolicy
    , VersionedScript
      PlutusV3
      plc110
      "failingValueContainsLeftNegativePolicyScript"
      V3_110.failingValueContainsLeftNegativePolicy
    , VersionedScript
      PlutusV3
      plc110
      "failingValueContainsRightNegativePolicyScript"
      V3_110.failingValueContainsRightNegativePolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingValueContainsReflexivePolicyScript"
      V3_110.succeedingValueContainsReflexivePolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingValueContainsDisjointPolicyScript"
      V3_110.succeedingValueContainsDisjointPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingValueContainsRightExtraKeyPolicyScript"
      V3_110.succeedingValueContainsRightExtraKeyPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingValueContainsRightHigherAmountPolicyScript"
      V3_110.succeedingValueContainsRightHigherAmountPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingValueContainsIsSubValuePolicyScript"
      V3_110.succeedingValueContainsIsSubValuePolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingValueContainsIsSubValueSmallerAmountPolicyScript"
      V3_110.succeedingValueContainsIsSubValueSmallerAmountPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingUnionValueEmptyIdentityPolicyScript"
      V3_110.succeedingUnionValueEmptyIdentityPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingUnionValueCommutativePolicyScript"
      V3_110.succeedingUnionValueCommutativePolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingUnionValueCommutativeSingleCoinPolicyScript"
      V3_110.succeedingUnionValueCommutativeSingleCoinPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingUnionValueAssociativePolicyScript"
      V3_110.succeedingUnionValueAssociativePolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingUnionValueAssociativeSingleCoinPolicyScript"
      V3_110.succeedingUnionValueAssociativeSingleCoinPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingUnionValueInversablePolicyScript"
      V3_110.succeedingUnionValueInversablePolicy
    , VersionedScript
      PlutusV3
      plc110
      "failingUnionValueOverflowPolicyScript"
      V3_110.failingUnionValueOverflowPolicy
    , VersionedScript
      PlutusV3
      plc110
      "failingUnionValueUnderflowPolicyScript"
      V3_110.failingUnionValueUnderflowPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingScaleValueZeroPolicyScript"
      V3_110.succeedingScaleValueZeroPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingScaleValuePositivePolicyScript"
      V3_110.succeedingScaleValuePositivePolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingScaleValueNegativePolicyScript"
      V3_110.succeedingScaleValueNegativePolicy
    , VersionedScript
      PlutusV3
      plc110
      "failingScaleValueOverflowPolicyScript"
      V3_110.failingScaleValueOverflowPolicy
    , VersionedScript
      PlutusV3
      plc110
      "failingScaleValueUnderflowPolicyScript"
      V3_110.failingScaleValueUnderflowPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingValueDataRoundTripPolicyScript"
      V3_110.succeedingValueDataRoundTripPolicy
    , VersionedScript
      PlutusV3
      plc110
      "failingUnValueDataInvalidDataPolicyScript"
      V3_110.failingUnValueDataInvalidDataPolicy
  , -- V3/1.0.0
    VersionedScript
      PlutusV3
      plc100
      "succeedingLookupMissingCoinPolicyScript"
      V3_100.succeedingLookupMissingCoinPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingInsertNewCoinPolicyScript"
      V3_100.succeedingInsertNewCoinPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingInsertExistingCoinPolicyScript"
      V3_100.succeedingInsertExistingCoinPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingDeleteExistingCoinPolicyScript"
      V3_100.succeedingDeleteExistingCoinPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingDeleteMissingCoinPolicyScript"
      V3_100.succeedingDeleteMissingCoinPolicy
    , VersionedScript
      PlutusV3
      plc100
      "failingInsertInvalidCurrencySymbolPolicyScript"
      V3_100.failingInsertInvalidCurrencySymbolPolicy
    , VersionedScript
      PlutusV3
      plc100
      "failingInsertInvalidTokenNamePolicyScript"
      V3_100.failingInsertInvalidTokenNamePolicy
    , VersionedScript
      PlutusV3
      plc100
      "failingInsertOverflowQuantityPolicyScript"
      V3_100.failingInsertOverflowQuantityPolicy
    , VersionedScript
      PlutusV3
      plc100
      "failingInsertUnderflowQuantityPolicyScript"
      V3_100.failingInsertUnderflowQuantityPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingValueContainsEmptyPolicyScript"
      V3_100.succeedingValueContainsEmptyPolicy
    , VersionedScript
      PlutusV3
      plc100
      "failingValueContainsLeftNegativePolicyScript"
      V3_100.failingValueContainsLeftNegativePolicy
    , VersionedScript
      PlutusV3
      plc100
      "failingValueContainsRightNegativePolicyScript"
      V3_100.failingValueContainsRightNegativePolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingValueContainsReflexivePolicyScript"
      V3_100.succeedingValueContainsReflexivePolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingValueContainsDisjointPolicyScript"
      V3_100.succeedingValueContainsDisjointPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingValueContainsRightExtraKeyPolicyScript"
      V3_100.succeedingValueContainsRightExtraKeyPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingValueContainsRightHigherAmountPolicyScript"
      V3_100.succeedingValueContainsRightHigherAmountPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingValueContainsIsSubValuePolicyScript"
      V3_100.succeedingValueContainsIsSubValuePolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingValueContainsIsSubValueSmallerAmountPolicyScript"
      V3_100.succeedingValueContainsIsSubValueSmallerAmountPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingUnionValueEmptyIdentityPolicyScript"
      V3_100.succeedingUnionValueEmptyIdentityPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingUnionValueCommutativePolicyScript"
      V3_100.succeedingUnionValueCommutativePolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingUnionValueCommutativeSingleCoinPolicyScript"
      V3_100.succeedingUnionValueCommutativeSingleCoinPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingUnionValueAssociativePolicyScript"
      V3_100.succeedingUnionValueAssociativePolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingUnionValueAssociativeSingleCoinPolicyScript"
      V3_100.succeedingUnionValueAssociativeSingleCoinPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingUnionValueInversablePolicyScript"
      V3_100.succeedingUnionValueInversablePolicy
    , VersionedScript
      PlutusV3
      plc100
      "failingUnionValueOverflowPolicyScript"
      V3_100.failingUnionValueOverflowPolicy
    , VersionedScript
      PlutusV3
      plc100
      "failingUnionValueUnderflowPolicyScript"
      V3_100.failingUnionValueUnderflowPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingScaleValueZeroPolicyScript"
      V3_100.succeedingScaleValueZeroPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingScaleValuePositivePolicyScript"
      V3_100.succeedingScaleValuePositivePolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingScaleValueNegativePolicyScript"
      V3_100.succeedingScaleValueNegativePolicy
    , VersionedScript
      PlutusV3
      plc100
      "failingScaleValueOverflowPolicyScript"
      V3_100.failingScaleValueOverflowPolicy
    , VersionedScript
      PlutusV3
      plc100
      "failingScaleValueUnderflowPolicyScript"
      V3_100.failingScaleValueUnderflowPolicy
    , VersionedScript
      PlutusV3
      plc100
      "succeedingValueDataRoundTripPolicyScript"
      V3_100.succeedingValueDataRoundTripPolicy
    , VersionedScript
      PlutusV3
      plc100
      "failingUnValueDataInvalidDataPolicyScript"
      V3_100.failingUnValueDataInvalidDataPolicy
  ]
