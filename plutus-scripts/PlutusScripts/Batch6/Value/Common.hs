-- editorconfig-checker-disable-file
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module PlutusScripts.Batch6.Value.Common (
  mkLookupMissingCoinPolicy,
  mkInsertNewCoinPolicy,
  mkInsertExistingCoinPolicy,
  mkDeleteExistingCoinPolicy,
  mkDeleteMissingCoinPolicy,
  mkInsertInvalidCurrencySymbolPolicy,
  mkInsertInvalidTokenNamePolicy,
  mkInsertOverflowQuantityPolicy,
  mkInsertUnderflowQuantityPolicy,
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
where

import PlutusTx.Builtins qualified as B
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Prelude ( Integer, ($), negate) 
import PlutusTx.Prelude qualified as P

-------------------------------------------------------------
-- LookupCoin tests
-------------------------------------------------------------

mkLookupMissingCoinPolicy :: P.BuiltinData -> P.BuiltinUnit
mkLookupMissingCoinPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      val = emptyValue
      expected = 0 :: Integer
   in if expected P.== BI.lookupCoin currSymbol tokenName val
        then BI.unitval
        else P.traceError "mkLookupMissingCoinPolicy"
{-# INLINEABLE mkLookupMissingCoinPolicy #-}

-------------------------------------------------------------
-- InsertCoin tests
-------------------------------------------------------------

mkInsertNewCoinPolicy :: P.BuiltinData -> P.BuiltinUnit
mkInsertNewCoinPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      amount = 100 :: Integer
      val = BI.insertCoin currSymbol tokenName amount emptyValue
      expected = amount
   in if expected P.== BI.lookupCoin currSymbol tokenName val
        then BI.unitval
        else P.traceError "mkInsertNewCoinPolicy"
{-# INLINEABLE mkInsertNewCoinPolicy #-}

mkInsertExistingCoinPolicy :: P.BuiltinData -> P.BuiltinUnit
mkInsertExistingCoinPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      amount = 100 :: Integer
      val =
        BI.insertCoin
          currSymbol
          tokenName
          amount
          (BI.insertCoin currSymbol tokenName amount emptyValue)
      expected = amount P.+ amount
   in if expected P.== BI.lookupCoin currSymbol tokenName val
          then BI.unitval
          else P.traceError "mkInsertExistingCoinPolicy"
{-# INLINEABLE mkInsertExistingCoinPolicy #-}

mkDeleteExistingCoinPolicy :: P.BuiltinData -> P.BuiltinUnit
mkDeleteExistingCoinPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      amount = 100 :: Integer
      deleteCoin = 0 :: Integer
      val =
        BI.insertCoin
          currSymbol
          tokenName
          deleteCoin
          (BI.insertCoin currSymbol tokenName amount emptyValue)
      expected = deleteCoin
   in if expected P.== BI.lookupCoin currSymbol tokenName val
        then BI.unitval
        else P.traceError "mkDeleteExistingCoinPolicy"
{-# INLINEABLE mkDeleteExistingCoinPolicy #-} 

mkDeleteMissingCoinPolicy :: P.BuiltinData -> P.BuiltinUnit
mkDeleteMissingCoinPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName1 = P.encodeUtf8 "bar"
      tokenName2 = P.encodeUtf8 "baz"
      amount = 100 :: Integer
      deleteCoin = 0 :: Integer
      val =
        BI.insertCoin
          currSymbol
          tokenName1
          deleteCoin
          (BI.insertCoin currSymbol tokenName2 amount emptyValue)
      expected = amount
    in if expected P.== BI.lookupCoin currSymbol tokenName2 val
          then BI.unitval
          else P.traceError "mkDeleteMissingCoinPolicy"
{-# INLINEABLE mkDeleteMissingCoinPolicy #-}

mkInsertInvalidCurrencySymbolPolicy :: P.BuiltinData -> P.BuiltinUnit
mkInsertInvalidCurrencySymbolPolicy _ctx =
  let invalidCurrSymbol = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" :: B.BuiltinByteString
      tokenName = P.encodeUtf8 "bar"
      amount = 100 :: Integer
      val = BI.insertCoin invalidCurrSymbol tokenName amount emptyValue -- Plinth is strict, should cause evaluation failure
   in BI.unitval
{-# INLINEABLE mkInsertInvalidCurrencySymbolPolicy #-}

mkInsertInvalidTokenNamePolicy :: P.BuiltinData -> P.BuiltinUnit
mkInsertInvalidTokenNamePolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      invalidTokenName = "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" :: B.BuiltinByteString
      amount = 100 :: Integer
      val = BI.insertCoin currSymbol invalidTokenName amount emptyValue -- Plinth is strict, should cause evaluation failure
   in BI.unitval
{-# INLINEABLE mkInsertInvalidTokenNamePolicy #-}

mkInsertOverflowQuantityPolicy :: P.BuiltinData -> P.BuiltinUnit
mkInsertOverflowQuantityPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      invalidAmount = 99999999999999999999999999999999999999999999999999 :: Integer 
      val = BI.insertCoin currSymbol tokenName invalidAmount emptyValue -- Plinth is strict, should cause evaluation failure
   in BI.unitval
{-# INLINEABLE mkInsertOverflowQuantityPolicy #-}

mkInsertUnderflowQuantityPolicy :: P.BuiltinData -> P.BuiltinUnit
mkInsertUnderflowQuantityPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      invalidAmount = negate 99999999999999999999999999999999999999999999999999 :: Integer
      val = BI.insertCoin currSymbol tokenName invalidAmount emptyValue -- Plinth is strict, should cause evaluation failure
   in BI.unitval
{-# INLINEABLE mkInsertUnderflowQuantityPolicy #-}

-------------------------------------------------------------
-- ValueContains tests
-------------------------------------------------------------

mkValueContainsEmptyPolicy :: P.BuiltinData -> P.BuiltinUnit
mkValueContainsEmptyPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      amount = 100 :: Integer
      val =
        BI.insertCoin
          currSymbol
          tokenName
          amount
          (BI.insertCoin currSymbol tokenName amount emptyValue)
   in if BI.valueContains emptyValue emptyValue P.&& BI.valueContains val emptyValue
        then BI.unitval
        else P.traceError "mkValueContainsEmptyPolicy"
{-# INLINEABLE mkValueContainsEmptyPolicy #-}

mkValueContainsLeftNegativePolicy :: P.BuiltinData -> P.BuiltinUnit
mkValueContainsLeftNegativePolicy _ctx =
  let currSymbol1 = P.encodeUtf8 "foo1"
      currSymbol2 = P.encodeUtf8 "foo2"
      tokenName1 = P.encodeUtf8 "bar1"
      tokenName2 = P.encodeUtf8 "bar2"
      amount1 = negate 100 :: Integer
      amount2 = 50 :: Integer
      leftVal = BI.insertCoin currSymbol1 tokenName1 amount1 emptyValue
      rightVal = BI.insertCoin currSymbol2 tokenName2 amount2 emptyValue
      result = BI.valueContains leftVal rightVal -- Plinth is strict, should cause evaluation failure
   in BI.unitval 
{-# INLINEABLE mkValueContainsLeftNegativePolicy #-}

mkValueContainsRightNegativePolicy :: P.BuiltinData -> P.BuiltinUnit
mkValueContainsRightNegativePolicy _ctx =
  let currSymbol1 = P.encodeUtf8 "foo1"
      currSymbol2 = P.encodeUtf8 "foo2"
      tokenName1 = P.encodeUtf8 "bar1"
      tokenName2 = P.encodeUtf8 "bar2"
      amount1 = 100 :: Integer
      amount2 = negate 50 :: Integer
      leftVal = BI.insertCoin currSymbol1 tokenName1 amount1 emptyValue
      rightVal = BI.insertCoin currSymbol2 tokenName2 amount2 emptyValue
      result = BI.valueContains leftVal rightVal -- Plinth is strict, should cause evaluation failure
   in BI.unitval 
{-# INLINEABLE mkValueContainsRightNegativePolicy #-}

mkValueContainsReflexivePolicy :: P.BuiltinData -> P.BuiltinUnit
mkValueContainsReflexivePolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      amount = 100 :: Integer
      val = BI.insertCoin currSymbol tokenName amount emptyValue
   in if BI.valueContains val val
        then BI.unitval
        else P.traceError "mkValueContainsReflexivePolicy"
{-# INLINEABLE mkValueContainsReflexivePolicy #-}

mkValueContainsDisjointPolicy :: P.BuiltinData -> P.BuiltinUnit
mkValueContainsDisjointPolicy _ctx =
  let currSymbol1 = P.encodeUtf8 "foo1"
      currSymbol2 = P.encodeUtf8 "foo2"
      tokenName1 = P.encodeUtf8 "bar1"
      tokenName2 = P.encodeUtf8 "bar2"
      amount1 = 100 :: Integer
      amount2 = 50 :: Integer
      leftVal = BI.insertCoin currSymbol1 tokenName1 amount1 emptyValue
      rightVal = BI.insertCoin currSymbol2 tokenName2 amount2 emptyValue
   in if P.not (BI.valueContains leftVal rightVal)
        then BI.unitval
        else P.traceError "mkValueContainsDisjointPolicy"
{-# INLINEABLE mkValueContainsDisjointPolicy #-}

mkValueContainsRightExtraKeyPolicy :: P.BuiltinData -> P.BuiltinUnit
mkValueContainsRightExtraKeyPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      currSymbol2 = P.encodeUtf8 "foo2"
      tokenName1 = P.encodeUtf8 "bar1"
      tokenName2 = P.encodeUtf8 "bar2"
      amount1Left = 100 :: Integer
      amount2Left = 50 :: Integer
      amount1Right = 70 :: Integer
      amount2Right = 80 :: Integer
      leftVal =
        BI.insertCoin currSymbol tokenName1 amount1Left $
          BI.insertCoin currSymbol tokenName2 amount2Left emptyValue
      rightVal =
        BI.insertCoin currSymbol tokenName1 amount1Right $
          BI.insertCoin currSymbol2 tokenName2 amount2Right emptyValue
   in if P.not (BI.valueContains leftVal rightVal)
        then BI.unitval
        else P.traceError "mkValueContainsRightExtraKeyPolicy"
{-# INLINEABLE mkValueContainsRightExtraKeyPolicy #-}

mkValueContainsRightHigherAmountPolicy :: P.BuiltinData -> P.BuiltinUnit
mkValueContainsRightHigherAmountPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      amountLeft = 100 :: Integer
      amountRight = 150 :: Integer
      leftVal = BI.insertCoin currSymbol tokenName amountLeft emptyValue
      rightVal = BI.insertCoin currSymbol tokenName amountRight emptyValue
   in if P.not (BI.valueContains leftVal rightVal)
        then BI.unitval
        else P.traceError "mkValueContainsRightHigherAmountPolicy"
{-# INLINEABLE mkValueContainsRightHigherAmountPolicy #-}

mkValueContainsIsSubValuePolicy :: P.BuiltinData -> P.BuiltinUnit
mkValueContainsIsSubValuePolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName1 = P.encodeUtf8 "bar1"
      tokenName2 = P.encodeUtf8 "bar2"
      amount1 = 100 :: Integer
      amount2 = 50 :: Integer
      leftVal =
        BI.insertCoin currSymbol tokenName1 amount1 $
          BI.insertCoin currSymbol tokenName2 amount2 emptyValue
      rightVal = BI.insertCoin currSymbol tokenName1 amount1 emptyValue
   in if BI.valueContains leftVal rightVal
        then BI.unitval
        else P.traceError "mkValueContainsIsSubValuePolicy"
{-# INLINEABLE mkValueContainsIsSubValuePolicy #-}

mkValueContainsIsSubValueSmallerAmountPolicy :: P.BuiltinData -> P.BuiltinUnit
mkValueContainsIsSubValueSmallerAmountPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName1 = P.encodeUtf8 "bar1"
      tokenName2 = P.encodeUtf8 "bar2"
      amount1 = 100 :: Integer
      amount2 = 50 :: Integer
      amount3 = 30 :: Integer
      leftVal =
        BI.insertCoin currSymbol tokenName1 amount1 $
          BI.insertCoin currSymbol tokenName2 amount2 emptyValue
      rightVal = BI.insertCoin currSymbol tokenName1 amount3 emptyValue
   in if BI.valueContains leftVal rightVal
        then BI.unitval
        else P.traceError "mkValueContainsIsSubValueSmallerAmountPolicy"
{-# INLINEABLE mkValueContainsIsSubValueSmallerAmountPolicy #-}

-------------------------------------------------------------
-- UnionValue tests
-------------------------------------------------------------

mkUnionValueEmptyIdentityPolicy :: P.BuiltinData -> P.BuiltinUnit
mkUnionValueEmptyIdentityPolicy _ctx =
  let val = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") 100 emptyValue
   in if val `valueEqual` BI.unionValue val emptyValue P.&& val `valueEqual` BI.unionValue emptyValue val
        then BI.unitval
        else P.traceError "mkUnionValueEmptyIdentityPolicy"
{-# INLINEABLE mkUnionValueEmptyIdentityPolicy #-}

mkUnionValueAssociativePolicy :: P.BuiltinData -> P.BuiltinUnit
mkUnionValueAssociativePolicy _ctx =
  let valA = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") 100 emptyValue
      valB = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "baz") 50 emptyValue
      valC = BI.insertCoin (P.encodeUtf8 "qux") (P.encodeUtf8 "quux") 25 emptyValue
      left = BI.unionValue valA (BI.unionValue valB valC)
      right = BI.unionValue (BI.unionValue valA valB) valC
   in if left `valueEqual` right
        then BI.unitval
        else P.traceError "mkUnionValueAssociativePolicy"
{-# INLINEABLE mkUnionValueAssociativePolicy #-}

mkUnionValueAssociativeSingleCoinPolicy :: P.BuiltinData -> P.BuiltinUnit
mkUnionValueAssociativeSingleCoinPolicy _ctx =
  let valA = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") 100 emptyValue
      valB = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") (-50) emptyValue
      valC = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") 25 emptyValue
      left = BI.unionValue valA (BI.unionValue valB valC)
      right = BI.unionValue (BI.unionValue valA valB) valC
   in if left `valueEqual` right
        then BI.unitval
        else P.traceError "mkUnionValueAssociativeSingleCoinPolicy"
{-# INLINEABLE mkUnionValueAssociativeSingleCoinPolicy #-}

mkUnionValueCommutativePolicy :: P.BuiltinData -> P.BuiltinUnit
mkUnionValueCommutativePolicy _ctx =
  let valA = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") (-100) emptyValue
      valB = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "baz") 50 emptyValue
      left = BI.unionValue valA valB
      right = BI.unionValue valB valA
   in if left `valueEqual` right
        then BI.unitval
        else P.traceError "mkUnionValueCommutativePolicy"
{-# INLINEABLE mkUnionValueCommutativePolicy #-}

mkUnionValueCommutativeSingleCoinPolicy :: P.BuiltinData -> P.BuiltinUnit
mkUnionValueCommutativeSingleCoinPolicy _ctx =
  let valA = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") (-100) emptyValue
      valB = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") 50 emptyValue
      left = BI.unionValue valA valB
      right = BI.unionValue valB valA
   in if left `valueEqual` right
        then BI.unitval
        else P.traceError "mkUnionValueCommutativeSingleCoinPolicy"
{-# INLINEABLE mkUnionValueCommutativeSingleCoinPolicy #-}

mkUnionValueInversablePolicy :: P.BuiltinData -> P.BuiltinUnit
mkUnionValueInversablePolicy _ctx =
  let val = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") 100 emptyValue
      negVal = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") (-100) emptyValue
      unioned = BI.unionValue val negVal
   in if unioned `valueEqual` emptyValue
        then BI.unitval
        else P.traceError "mkUnionValueInversablePolicy"
{-# INLINEABLE mkUnionValueInversablePolicy #-}

mkUnionValueOverflowPolicy :: P.BuiltinData -> P.BuiltinUnit
mkUnionValueOverflowPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      val1 = BI.insertCoin currSymbol tokenName 170141183460469231731687303715884105727 emptyValue
      val2 = BI.insertCoin currSymbol tokenName 1 emptyValue
      unioned = BI.unionValue val1 val2 -- Plinth is strict, should cause evaluation failure
   in BI.unitval
{-# INLINEABLE mkUnionValueOverflowPolicy #-}

mkUnionValueUnderflowPolicy :: P.BuiltinData -> P.BuiltinUnit
mkUnionValueUnderflowPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      val1 = BI.insertCoin currSymbol tokenName (negate 170141183460469231731687303715884105728) emptyValue
      val2 = BI.insertCoin currSymbol tokenName (negate 1) emptyValue
      unioned = BI.unionValue val1 val2 -- Plinth is strict, should cause evaluation failure
   in BI.unitval
{-# INLINEABLE mkUnionValueUnderflowPolicy #-}

-------------------------------------------------------------
-- ScaleValue tests
-------------------------------------------------------------

mkScaleValueZeroPolicy :: P.BuiltinData -> P.BuiltinUnit
mkScaleValueZeroPolicy _ctx =
  let val = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") 100 emptyValue
      scaled = BI.scaleValue 0 val
   in if scaled `valueEqual` emptyValue
        then BI.unitval
        else P.traceError "mkScaleValueZeroPolicy"
{-# INLINEABLE mkScaleValueZeroPolicy #-}

mkScaleValuePositivePolicy :: P.BuiltinData -> P.BuiltinUnit
mkScaleValuePositivePolicy _ctx =
  let val = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") 100 emptyValue
      factor = 3 :: Integer
      scaled = BI.scaleValue factor val
      expected = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") (100 P.* factor) emptyValue
   in if scaled `valueEqual` expected
        then BI.unitval
        else P.traceError "mkScaleValuePositivePolicy"
{-# INLINEABLE mkScaleValuePositivePolicy #-}

mkScaleValueNegativePolicy :: P.BuiltinData -> P.BuiltinUnit
mkScaleValueNegativePolicy _ctx =
  let val = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") 100 emptyValue
      factor = -2 :: Integer
      scaled = BI.scaleValue factor val
      expected = BI.insertCoin (P.encodeUtf8 "foo") (P.encodeUtf8 "bar") (100 P.* factor) emptyValue
   in if scaled `valueEqual` expected
        then BI.unitval
        else P.traceError "mkScaleValueNegativePolicy"
{-# INLINEABLE mkScaleValueNegativePolicy #-}

mkScaleValueOverflowPolicy :: P.BuiltinData -> P.BuiltinUnit
mkScaleValueOverflowPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      val = BI.insertCoin currSymbol tokenName 85070591730234615865843651857942052864 emptyValue
      factor = 2 :: Integer
      scaled = BI.scaleValue factor val -- Plinth is strict, should cause evaluation failure
   in BI.unitval
{-# INLINEABLE mkScaleValueOverflowPolicy #-}

mkScaleValueUnderflowPolicy :: P.BuiltinData -> P.BuiltinUnit
mkScaleValueUnderflowPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      val = BI.insertCoin currSymbol tokenName (negate 85070591730234615865843651857942052865) emptyValue
      factor = 2 :: Integer
      scaled = BI.scaleValue factor val -- Plinth is strict, should cause evaluation failure
   in BI.unitval
{-# INLINEABLE mkScaleValueUnderflowPolicy #-}

-------------------------------------------------------------
-- ValueData/UnValueData tests
-------------------------------------------------------------

mkValueDataRoundTripPolicy :: P.BuiltinData -> P.BuiltinUnit
mkValueDataRoundTripPolicy _ctx =
  let currSymbol = P.encodeUtf8 "foo"
      tokenName = P.encodeUtf8 "bar"
      amount = 100 :: Integer
      val = BI.insertCoin currSymbol tokenName amount emptyValue
      valData = BI.mkValue val
      valRoundTripped = BI.unsafeDataAsValue valData
   in if val `valueEqual` valRoundTripped
        then BI.unitval
        else P.traceError "mkValueDataRoundTripPolicy"
{-# INLINEABLE mkValueDataRoundTripPolicy #-}

mkUnValueDataInvalidDataPolicy :: P.BuiltinData -> P.BuiltinUnit
mkUnValueDataInvalidDataPolicy _ctx =
  let invalidData = BI.mkList (P.toOpaque [B.mkI 1, B.mkI 2])
      val = BI.unsafeDataAsValue invalidData -- Plinth is strict, should cause evaluation failure
   in BI.unitval
{-# INLINEABLE mkUnValueDataInvalidDataPolicy #-}

-------------------------------------------------------------
-- Helpers
-------------------------------------------------------------

emptyValue :: BI.BuiltinValue
emptyValue = BI.unsafeDataAsValue $ B.mkMap []
{-# INLINEABLE emptyValue #-}

-- (Value, union) is antisymmetric
valueEqual :: BI.BuiltinValue -> BI.BuiltinValue -> P.Bool
valueEqual v1 v2 = BI.valueContains v1 v2 P.&& BI.valueContains v2 v1
{-# INLINEABLE valueEqual #-}
