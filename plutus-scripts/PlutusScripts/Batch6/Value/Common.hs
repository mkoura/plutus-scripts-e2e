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

emptyValue :: BI.BuiltinValue
emptyValue = BI.unsafeDataAsValue $ BI.mkMap HO.mkNil
{-# INLINEABLE emptyValue #-}
