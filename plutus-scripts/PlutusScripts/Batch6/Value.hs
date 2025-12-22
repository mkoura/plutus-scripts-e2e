module PlutusScripts.Batch6.Value (v3ValueScripts) where

import Helpers.Envelopes (VersionedScript (VersionedScript), plc110)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))
import PlutusScripts.Batch6.Value.V3_110 qualified as V3_110
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
  ]
