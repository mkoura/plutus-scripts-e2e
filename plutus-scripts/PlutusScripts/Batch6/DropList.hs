{-| Simple end-to-end tests for the Plutus Core `dropList` builtin.  These are
adapted from the `plutus-conformance` tests. -}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module PlutusScripts.Batch6.DropList
  {-
  (
  mkCountSetBitsPolicy,
  mkFindFirstSetBitPolicy,
  succeedingCountSetBitsParams,
  succeedingFindFirstSetBitParams
  )
-}
where

import PlutusTx.Prelude qualified as P
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval, BuiltinInteger)

import PlutusScripts.Helpers (hxs)

data ParamsI = ParamsI
  { countI :: Integer
  , inputI :: [Integer]
  , outputI :: [Integer]
  }
PlutusTx.unstableMakeIsData ''ParamsI
PlutusTx.makeLift ''ParamsI
  
data ParamsBS = ParamsBS
  { countBS :: Integer
  , inputBS :: [P.BuiltinByteString]
  , outputBS :: [P.BuiltinByteString]
  }
PlutusTx.unstableMakeIsData ''ParamsBS
PlutusTx.makeLift ''ParamsBS

{-# INLINEABLE bilEqI #-}
bilEqI :: P.BuiltinList BI.BuiltinInteger -> P.BuiltinList BI.BuiltinInteger -> Bool
bilEqI as bs = (BI.head as P.== BI.head bs) && bilEqI (BI.tail as) (BI.tail bs)

{-# INLINEABLE bilEqB #-}
bilEqB :: P.BuiltinList BI.BuiltinByteString -> P.BuiltinList BI.BuiltinByteString -> Bool
bilEqB as bs = (BI.head as P.== BI.head bs) && bilEqB (BI.tail as) (BI.tail bs)

{-# INLINEABLE mkDropListPolicyI #-}
mkDropListPolicyI :: [ParamsI] -> P.BuiltinData -> P.BuiltinUnit
mkDropListPolicyI l _ctx = go l
  where go [] = BI.unitval
        go (ParamsI{..}:rest) =
          if BI.drop countI (P.toOpaque inputI) `bilEqI` (P.toOpaque outputI)
          then go rest
          else P.traceError "mkCountSetBitsPolicyI"
{-
{-# INLINEABLE mkDropListPolicyBS #-}
mkDropListPolicyBS :: [ParamsBS] -> P.BuiltinData -> P.BuiltinUnit
mkDropListPolicyBS l _ctx = go l
  where go [] = BI.unitval
        go (ParamsBS{..}:rest) =
          if BI.drop countBS (P.toBuiltin inputBS) `bilEqB` P.toBuiltin outputBS
          then go rest
          else P.traceError "mkCountSetBitsPolicyB"
-}

-- Succeeding inputs for lists of UPLC integers; `dropList` can only fail for cost reasons.
succeedingDropListParamsI :: [ParamsI]
succeedingDropListParamsI =
  [ ParamsI  -- Dropping zero elements has no effect.
    { countI = 0
    , inputI = [11,22,33,44,55,66,77,88,99]
    , outputI = [11,22,33,44,55,66,77,88,99]
    },
    ParamsI  -- A typical case where you really drop some elements.
    { countI = 7
    , inputI = [11,22,33,44,55,66,77,88,99]
    , outputI = [88,99]
    },
    ParamsI  -- Dropping more elements than the length of the list succeeds and returns the empty list.
    { countI = 17
    , inputI = [11,22,33,44,55,66,77,88,99]
    , outputI = []
    },
    ParamsI  -- Dropping a negative number of elements succeeds and returns the list unchanged.
    { countI = 07
    , inputI = [11,22,33,44,55,66,77,88,99]
    , outputI = [11,22,33,44,55,66,77,88,99]
    },
    -- Dropping any number of elements from an empty list always succeeds (and
    -- returns the empty list).
    ParamsI
    { countI = -1234
    , inputI = []
    , outputI = []
    },
    -- Dropping any number of elements from an empty list always succeeds (and
    -- returns the empty list).
    ParamsI
    { countI = 0
    , inputI = []
    , outputI = []
    },
    -- Dropping any number of elements from an empty list always succeeds (and
    -- returns the empty list).
    ParamsI
    { countI = 1234
    , inputI = []
    , outputI = []
    }
  ]

-- Failing inputs for lists of UPLC integers.
---These should return the empty list.  However, if run in restricting mode they
-- will (probably, depending on the cost model) attempt to consume the maxmimum
-- budget and fail because the cost depends on the absolute value of the `count`
-- argument, irrespective of the size of the list.
failingDropListParamsInteger :: [ParamsI]
failingDropListParamsInteger =
  [ ParamsI
    { countI = 10000000000000000000
    , inputI = [11,22,33,44,55,66,77,88,99]
    , outputI = []
    },
    ParamsI
    { countI = -12345678901234567890
    , inputI = [11,22,33,44,55,66,77,88,99]
    , outputI = []
    } ,
    ParamsI  -- Drop (maxBound::Int)-1 elements
    { countI = 9223372036854775806
    , inputI = [ 20, 21, 22, 23, 24, 25, 26, 27, 28, 29
             , 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
             , 40, 41, 42, 43, 44, 45, 46, 47, 48, 49
             , 50, 51, 52, 53, 54, 55, 56, 57, 58, 59
             , 60, 61, 62, 63, 64, 65, 66, 67, 68, 69
             , 70, 71, 72, 73, 74, 75, 76, 77, 78, 79
             , 80, 81, 82, 83, 84, 85, 86, 87, 88, 89
             , 90, 91, 92, 93, 94, 95, 96, 97, 98, 99
             , 100, 101, 102, 103, 104, 105, 106, 107
             , 108, 109, 110, 111 ]
    , outputI = []
    },
    ParamsI  -- Drop (maxBound::Int) elements
    { countI = 9223372036854775807
    , inputI = [ 20, 21, 22, 23, 24, 25, 26, 27, 28, 29
             , 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
             , 40, 41, 42, 43, 44, 45, 46, 47, 48, 49
             , 50, 51, 52, 53, 54, 55, 56, 57, 58, 59
             , 60, 61, 62, 63, 64, 65, 66, 67, 68, 69
             , 70, 71, 72, 73, 74, 75, 76, 77, 78, 79
             , 80, 81, 82, 83, 84, 85, 86, 87, 88, 89
             , 90, 91, 92, 93, 94, 95, 96, 97, 98, 99
             , 100, 101, 102, 103, 104, 105, 106, 107
             , 108, 109, 110, 111 ]
    , outputI = []
    },
    ParamsI  -- Drop (maxBound::Int)+1 elements
    { countI = 9223372036854775808
    , inputI = [ 20, 21, 22, 23, 24, 25, 26, 27, 28, 29
             , 30, 31, 32, 33, 34, 35, 36, 37, 38, 39
             , 40, 41, 42, 43, 44, 45, 46, 47, 48, 49
             , 50, 51, 52, 53, 54, 55, 56, 57, 58, 59
             , 60, 61, 62, 63, 64, 65, 66, 67, 68, 69
             , 70, 71, 72, 73, 74, 75, 76, 77, 78, 79
             , 80, 81, 82, 83, 84, 85, 86, 87, 88, 89
             , 90, 91, 92, 93, 94, 95, 96, 97, 98, 99
             , 100, 101, 102, 103, 104, 105, 106, 107
             , 108, 109, 110, 111 ]
    , outputI = []
    }
  ]

succeedingDropListParamsBS :: [ParamsBS]
succeedingDropListParamsBS =
  [ ParamsBS  -- Dropping zero elements has no effect.
    { countBS = 0
    , inputBS = [ hxs "123456"
                , hxs "234567"
                , hxs "345678"
                , hxs "456789"
                , hxs "56789a"
                , hxs "6789ab"
                , hxs "789abc"
                , hxs "89abcd93232479173417824fff"
                , hxs "ffeeffeeffee"
                ]
    , outputBS = [ hxs "123456"
                 , hxs "234567"
                 , hxs "345678"
                 , hxs "456789"
                 , hxs "56789a"
                 , hxs "6789ab"
                 , hxs "789abc"
                 , hxs "89abcd93232479173417824fff"
                 , hxs "ffeeffeeffee"
                 ]
    },
    ParamsBS
    { countBS = 7
    , inputBS = [ hxs "123456"
                , hxs "234567"
                , hxs "345678"
                , hxs "456789"
                , hxs "56789a"
                , hxs "6789ab"
                , hxs "789abc"
                , hxs "89abcd93232479173417824fff"
                , hxs "ffeeffeeffee"
               ]
    , outputBS = [ hxs "89abcd93232479173417824fff"
                 , hxs "ffeeffeeffee"
                 ]
    },
    -- Dropping more elements than the length of the inputBS succeeds and returns the
    -- empty inputBS.
    ParamsBS
    { countBS = 17
    , inputBS = [ hxs "123456"
                , hxs "234567"
                , hxs "345678"
                , hxs "456789"
                , hxs "56789a"
                , hxs "6789ab"
                , hxs "789abc"
                , hxs "89abcd93232479173417824fff"
                , hxs "ffeeffeeffee"
                ]
    , outputBS = []
    },
    ParamsBS  -- Dropping a negative number of elements succeeds and returns the inputBS unchanged.
    { countBS = -7
    , inputBS = [ hxs "123456"
                , hxs "234567"
                , hxs "345678"
                , hxs "456789"
                , hxs "56789a"
                , hxs "6789ab"
                , hxs "789abc"
                , hxs "89abcd93232479173417824fff"
                , hxs "ffeeffeeffee"
                ]
    , outputBS = [ hxs "123456"
                 , hxs "234567"
                 , hxs "345678"
                 , hxs "456789"
                 , hxs "56789a"
                 , hxs "6789ab"
                 , hxs "789abc"
                 , hxs "89abcd93232479173417824fff"
                 , hxs "ffeeffeeffee"
                 ]
    }
  ]
