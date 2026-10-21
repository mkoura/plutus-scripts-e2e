-- | Minting policies for CIP-0156.
module PlutusScripts.Batch7.MultiIndexArray.Common (
  mkIntegerPolicy,
  mkDataPolicy,
  mkRepeatedPolicy,
  mkFailingPolicy,
  mkOverLimitPolicy,
  repeatedSelections,
  failingLookups,
) where

import Data.Text (Text)
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)
import PlutusTx.List qualified as List
import PlutusTx.Prelude qualified as P

-- | Check ordering, duplicates, the first and last elements, and empty requests.
{-# INLINEABLE mkIntegerPolicy #-}
mkIntegerPolicy :: P.BuiltinData -> P.BuiltinUnit
mkIntegerPolicy _ctx =
  let arr = BI.listToArray (P.toOpaque [10 :: Integer, 20, 30, 40])
      selected = P.fromOpaque (BI.multiIndexArray arr (P.toOpaque ([3, 0, 2, 3] :: [Integer])))
      noElements = BI.multiIndexArray arr (P.toOpaque ([] :: [Integer]))
      emptyArray = BI.listToArray (P.toOpaque ([] :: [Integer]))
      emptyBoth = BI.multiIndexArray emptyArray (P.toOpaque ([] :: [Integer]))
   in if selected
        P.== [40 :: Integer, 10, 30, 40]
        P.&& BI.null noElements
        P.&& BI.null emptyBoth
        then BI.unitval
        else P.traceError "multiIndexArray integer selection"

-- | Select heterogeneous Data without decoding or changing its contents.
{-# INLINEABLE mkDataPolicy #-}
mkDataPolicy :: P.BuiltinData -> P.BuiltinUnit
mkDataPolicy _ctx =
  let number = P.toBuiltinData (42 :: Integer)
      bytes = P.toBuiltinData ("payload" :: P.BuiltinByteString)
      nested = P.toBuiltinData ([1, 2, 3] :: [Integer])
      arr = BI.listToArray (P.toOpaque [number, bytes, nested])
      selected = P.fromOpaque (BI.multiIndexArray arr (P.toOpaque ([2, 0, 2, 1] :: [Integer])))
   in if selected P.== [nested, number, nested, bytes]
        then BI.unitval
        else P.traceError "multiIndexArray Data selection"

-- | Construct repeated indices at runtime, keeping the script small at the limit.
{-# INLINEABLE mkRepeatedPolicy #-}
mkRepeatedPolicy :: Integer -> P.BuiltinData -> P.BuiltinUnit
mkRepeatedPolicy count _ctx =
  let arr = BI.listToArray (P.toOpaque [42 :: Integer])
      indices = P.toOpaque (List.replicate count (0 :: Integer))
      selected = P.fromOpaque (BI.multiIndexArray arr indices) :: [Integer]
   in if List.length selected P.== count P.&& List.all (P.== 42) selected
        then BI.unitval
        else P.traceError "multiIndexArray repeated selection"

{-| Accept ANY list the builtin returns, so that only the builtin can fail.
An unexpected successful lookup must succeed as a policy, so the E2E test
cannot mistake a policy assertion failure for a builtin failure. The builtin
fails when applied; the length check only keeps its result in use. -}
{-# INLINEABLE mkFailingPolicy #-}
mkFailingPolicy :: [Integer] -> [Integer] -> P.BuiltinData -> P.BuiltinUnit
mkFailingPolicy elements indices _ctx =
  let arr = BI.listToArray (P.toOpaque elements)
      selected = P.fromOpaque (BI.multiIndexArray arr (P.toOpaque indices)) :: [Integer]
   in if List.length selected P.>= 0
        then BI.unitval
        else P.traceError "unreachable negative length"

-- | All indices are in bounds; only the 1,024-index limit may reject this call.
{-# INLINEABLE mkOverLimitPolicy #-}
mkOverLimitPolicy :: P.BuiltinData -> P.BuiltinUnit
mkOverLimitPolicy = mkFailingPolicy [42] (List.replicate 1025 0)

-- | Script names and index counts for 'mkRepeatedPolicy'; 1,024 is the inclusive limit.
repeatedSelections :: [(Text, Integer)]
repeatedSelections =
  [ ("succeedingMultiIndexArrayRepeated1PolicyScript", 1)
  , ("succeedingMultiIndexArrayRepeated64PolicyScript", 64)
  , ("succeedingMultiIndexArrayRepeated1024PolicyScript", 1024)
  ]

{-| Script names, array elements and indices for 'mkFailingPolicy'.
The huge indices are 2^64 and -2^64: narrowing either to a 64-bit machine
integer before the bounds check would yield 0, which is in bounds. -}
failingLookups :: [(Text, [Integer], [Integer])]
failingLookups =
  [ ("failingMultiIndexArrayNegativePolicyScript", [10, 20, 30], [-1])
  , ("failingMultiIndexArrayUpperBoundPolicyScript", [10, 20, 30], [3])
  , ("failingMultiIndexArrayEmptyPolicyScript", [], [0])
  , ("failingMultiIndexArrayLateInvalidPolicyScript", [10, 20, 30], [0, 1, 3])
  , ("failingMultiIndexArrayHugePositivePolicyScript", [10, 20, 30], [18446744073709551616])
  , ("failingMultiIndexArrayHugeNegativePolicyScript", [10, 20, 30], [-18446744073709551616])
  ]
