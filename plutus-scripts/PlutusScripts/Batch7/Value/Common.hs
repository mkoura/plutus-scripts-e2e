-- | Runtime checks for the CIP-0168 Value builtins.
module PlutusScripts.Batch7.Value.Common (
  mkPoliciesPolicy,
  mkAssetCountPolicy,
  mkFilterPolicy,
  mkFailingFilterPolicy,
  mkSizePolicy,
  checkTransition,
) where

import PlutusTx.Builtins qualified as B
import PlutusTx.Builtins.Internal qualified as BI (BuiltinValue, unitval)
import PlutusTx.List qualified as List
import PlutusTx.Prelude qualified as P

{-# INLINEABLE emptyValue #-}
emptyValue :: BI.BuiltinValue
emptyValue = B.unsafeDataAsValue (B.mkMap [])

{-# INLINEABLE valueEqual #-}
valueEqual :: BI.BuiltinValue -> BI.BuiltinValue -> Bool
valueEqual a b = B.mkValue a P.== B.mkValue b

{-# INLINEABLE fromCoins #-}
fromCoins :: [(P.BuiltinByteString, P.BuiltinByteString, Integer)] -> BI.BuiltinValue
fromCoins = List.foldr (\(p, t, q) -> B.insertCoin p t q) emptyValue

{-# INLINEABLE check #-}
check :: Bool -> P.BuiltinUnit
check ok = if ok then BI.unitval else P.traceError "value builtin assertion"

{-# INLINEABLE mkPoliciesPolicy #-}
mkPoliciesPolicy
  :: [(P.BuiltinByteString, P.BuiltinByteString, Integer)]
  -> [P.BuiltinByteString]
  -> P.BuiltinData
  -> P.BuiltinUnit
mkPoliciesPolicy coins expected _ctx =
  check (P.fromOpaque (B.policies (fromCoins coins)) P.== expected)

-- | Counts include ADA, ignore quantities, and update after deletion/cancellation.
{-# INLINEABLE mkAssetCountPolicy #-}
mkAssetCountPolicy :: P.BuiltinData -> P.BuiltinUnit
mkAssetCountPolicy _ctx =
  let ada = B.insertCoin "" "" 100 emptyValue
      v = B.insertCoin "a" "x" (-7) (B.insertCoin "a" "y" 999 ada)
      deleted = B.insertCoin "a" "x" 0 v
      cancelled = B.unionValue v (B.scaleValue (-1) v)
   in check
        ( B.assetCount emptyValue
            P.== 0
            P.&& B.assetCount ada
            P.== 1
            P.&& B.assetCount v
            P.== 3
            P.&& B.assetCount deleted
            P.== 2
            P.&& B.assetCount cancelled
            P.== 0
            P.&& valueEqual cancelled emptyValue
        )

{-# INLINEABLE mkFilterPolicy #-}
mkFilterPolicy
  :: (P.BuiltinList P.BuiltinByteString -> BI.BuiltinValue -> BI.BuiltinValue)
  -> [P.BuiltinByteString]
  -> [(P.BuiltinByteString, P.BuiltinByteString, Integer)]
  -> [(P.BuiltinByteString, P.BuiltinByteString, Integer)]
  -> P.BuiltinData
  -> P.BuiltinUnit
mkFilterPolicy filterValue ids coins expected _ctx =
  let v = fromCoins coins
      ps = P.toOpaque ids
      result = filterValue ps v
   in check (valueEqual result (fromCoins expected))

{-| Observe a returned Value but accept EVERY possible result. A mutant filter
returning any Value must make this policy succeed, exposing unexpected success.
Using the serialised Data rather than another Value builtin isolates failure. -}
{-# INLINEABLE mkFailingFilterPolicy #-}
mkFailingFilterPolicy
  :: (P.BuiltinList P.BuiltinByteString -> BI.BuiltinValue -> BI.BuiltinValue)
  -> [P.BuiltinByteString]
  -> [(P.BuiltinByteString, P.BuiltinByteString, Integer)]
  -> P.BuiltinData
  -> P.BuiltinUnit
mkFailingFilterPolicy filterValue ids coins _ctx =
  let v = fromCoins coins
      result = filterValue (P.toOpaque ids) v
      observed = B.mkValue result
   in if B.lengthOfByteString (B.serialiseData observed) P.>= 0
        then BI.unitval
        else P.traceError "unreachable negative serialised length"

-- | Whole-policy size profiles: construction and result checking are included.
{-# INLINEABLE mkSizePolicy #-}
mkSizePolicy :: Bool -> Integer -> P.BuiltinData -> P.BuiltinUnit
mkSizePolicy samePolicy count _ctx =
  let coins =
        List.map
          ( \i ->
              ( if samePolicy then "p" else B.integerToByteString B.BigEndian 2 i
              , B.integerToByteString B.BigEndian 2 i
              , i P.+ 1
              )
          )
          (P.enumFromTo 1 count)
      v = fromCoins coins
      ids = if samePolicy then ["p"] else List.map (\(p, _, _) -> p) coins
      kept = B.keepPolicies (P.toOpaque ids) v
      dropped = B.dropPolicies (P.toOpaque ids) v
   in check
        ( B.assetCount v
            P.== count
            P.&& List.length (P.fromOpaque (B.policies v))
            P.== (if samePolicy then 1 else count)
            P.&& valueEqual kept v
            P.&& valueEqual dropped emptyValue
        )

{-# INLINEABLE dual #-}
dual :: [P.BuiltinByteString] -> BI.BuiltinValue -> Bool
dual ids v =
  valueEqual
    ( B.unionValue
        (B.keepPolicies (P.toOpaque ids) v)
        (B.dropPolicies (P.toOpaque ids) v)
    )
    v

{-# INLINEABLE ordered #-}
ordered :: [P.BuiltinByteString] -> Bool
ordered xs = case xs of
  [] -> True
  [_] -> True
  a : b : rest -> a P.< b P.&& ordered (b : rest)

-- | Shared predicate for the V3 and V4 spending-context wrappers.
{-# INLINEABLE checkTransition #-}
checkTransition
  :: Integer
  -> P.BuiltinByteString
  -> Integer
  -> BI.BuiltinValue
  -> BI.BuiltinValue
  -> Bool
checkTransition mode protocol expected before after =
  let oldProtocol = B.keepPolicies (P.toOpaque [protocol]) before
      newProtocol = B.keepPolicies (P.toOpaque [protocol]) after
      preserved =
        if mode P.== 0
          then valueEqual oldProtocol newProtocol
          else
            mode
              P.== 1
              P.&& valueEqual
                (B.dropPolicies (P.toOpaque ([""] :: [P.BuiltinByteString])) before)
                (B.dropPolicies (P.toOpaque ([""] :: [P.BuiltinByteString])) after)
   in protocol
        P./= ""
        P.&& expected
        P.> 0
        P.&& P.fromOpaque (B.policies oldProtocol)
        P.== [protocol]
        P.&& B.assetCount oldProtocol
        P.== expected
        P.&& B.assetCount newProtocol
        P.== expected
        P.&& preserved
        P.&& ordered (P.fromOpaque (B.policies before))
        P.&& ordered (P.fromOpaque (B.policies after))
        P.&& countPartition before
        P.&& countPartition after
        P.&& adaPartition before
        P.&& adaPartition after
        P.&& dual [""] before
        P.&& dual [""] after
        P.&& dual [protocol] before
        P.&& dual [protocol] after

-- | Distinct assets split exactly into the ADA and non-ADA partitions.
{-# INLINEABLE countPartition #-}
countPartition :: BI.BuiltinValue -> Bool
countPartition v =
  B.assetCount v
    P.== B.assetCount (B.keepPolicies (P.toOpaque ([""] :: [P.BuiltinByteString])) v)
    P.+ B.assetCount (B.dropPolicies (P.toOpaque ([""] :: [P.BuiltinByteString])) v)

{-# INLINEABLE adaPartition #-}
adaPartition :: BI.BuiltinValue -> Bool
adaPartition v =
  let ada = B.keepPolicies (P.toOpaque ([""] :: [P.BuiltinByteString])) v
   in B.assetCount ada
        P.== 1
        P.&& B.lookupCoin "" "" ada
        P.== B.lookupCoin "" "" v
