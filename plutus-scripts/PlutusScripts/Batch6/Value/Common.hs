-- editorconfig-checker-disable-file

module PlutusScripts.Batch6.Value.Common (
  mkInsertNewCoinPolicy,
  mkInsertExistingCoinPolicy,
  mkDeleteExistingCoinPolicy,
  mkDeleteMissingCoinPolicy,
)
where

import PlutusTx.Builtins qualified as B
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Builtins.HasOpaque qualified as HO
import PlutusTx.Prelude qualified as P

-- A key larger than 32-bit, which is not allowed
-- (program 1.0.0
--   [ (builtin insertCoin)
--     (con bytestring #aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)
--     (con bytestring #)
--     (con integer 1)
--     (con value [])
--   ]
-- )

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
      amount1 = -100 :: Integer
      amount2 = 50 :: Integer
      leftVal = BI.insertCoin currSymbol1 tokenName1 amount1 emptyValue
      rightVal = BI.insertCoin currSymbol2 tokenName2 amount2 emptyValue
   in if BI.valueContains leftVal rightVal -- should cause evaluation failure
        then BI.unitval
        else P.traceError "mkValueContainsLeftNegativePolicy"
{-# INLINEABLE mkValueContainsLeftNegativePolicy #-}

mkValueContainsRightNegativePolicy :: P.BuiltinData -> P.BuiltinUnit
mkValueContainsRightNegativePolicy _ctx =
  let currSymbol1 = P.encodeUtf8 "foo1"
      currSymbol2 = P.encodeUtf8 "foo2"
      tokenName1 = P.encodeUtf8 "bar1"
      tokenName2 = P.encodeUtf8 "bar2"
      amount1 = 100 :: Integer
      amount2 = -50 :: Integer
      leftVal = BI.insertCoin currSymbol1 tokenName1 amount1 emptyValue
      rightVal = BI.insertCoin currSymbol2 tokenName2 amount2 emptyValue
   in if BI.valueContains leftVal rightVal -- should cause evaluation failure
        then BI.unitval
        else P.traceError "mkValueContainsRightNegativePolicy"
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

-------------------------------------------------------------
-- ScaleValue tests
-------------------------------------------------------------

-------------------------------------------------------------
-- Helpers
-------------------------------------------------------------

emptyValue :: BI.BuiltinValue
emptyValue = BI.unsafeDataAsValue $ BI.mkMap HO.mkNil
{-# INLINEABLE emptyValue #-}

-- (Value, union) is antisymmetric
valueEqual :: BI.BuiltinValue -> BI.BuiltinValue -> P.Bool
valueEqual v1 v2 = BI.valueContains v1 v2 P.&& BI.valueContains v2 v1
{-# INLINEABLE valueEqual #-}
