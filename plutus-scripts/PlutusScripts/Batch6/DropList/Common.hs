{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- | Simple end-to-end tests for the Plutus Core `dropList` builtin.  These are
adapted from the `plutus-conformance` tests.
-}
module PlutusScripts.Batch6.DropList.Common (mkDropListPolicy, succeedingDropListParams, expensiveDropListParams)
where

import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)
import PlutusTx.Prelude qualified as P

data Params = Params
  { count :: Integer
  , input :: [Integer]
  , output :: [Integer]
  }
PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

{-# INLINEABLE eqIntList #-}
eqIntList :: P.BuiltinList Integer -> P.BuiltinList Integer -> Bool
eqIntList as bs =
  if BI.null as && BI.null bs
    then True
    else (BI.head as P.== BI.head bs) && eqIntList (BI.tail as) (BI.tail bs)

{-# INLINEABLE mkDropListPolicy #-}
mkDropListPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkDropListPolicy l _ctx = go l
 where
  go [] = BI.unitval
  go (Params{..} : rest) =
    if BI.drop count (P.toOpaque input) `eqIntList` (P.toOpaque output)
      then go rest
      else P.traceError "mkDropListPolicy"

-- Succeeding inputs for lists of UPLC integers; `dropList` can only fail for cost reasons.
succeedingDropListParams :: [Params]
succeedingDropListParams =
  [ Params -- Dropping zero elements has no effect.
      { count = 0
      , input = [11, 22, 33, 44, 55, 66, 77, 88, 99]
      , output = [11, 22, 33, 44, 55, 66, 77, 88, 99]
      }
  , Params -- A typical case where you really drop some elements.
      { count = 7
      , input = [11, 22, 33, 44, 55, 66, 77, 88, 99]
      , output = [88, 99]
      }
  , Params -- Dropping more elements than the length of the list succeeds and returns the empty list.
      { count = 17
      , input = [11, 22, 33, 44, 55, 66, 77, 88, 99]
      , output = []
      }
  , Params -- Dropping a negative number of elements succeeds and returns the list unchanged.
      { count = 07
      , input = [11, 22, 33, 44, 55, 66, 77, 88, 99]
      , output = [11, 22, 33, 44, 55, 66, 77, 88, 99]
      }
  , -- Dropping any number of elements from an empty list always succeeds (and
    -- returns the empty list).
    Params
      { count = -1234
      , input = []
      , output = []
      }
  , -- Dropping any number of elements from an empty list always succeeds (and
    -- returns the empty list).
    Params
      { count = 0
      , input = []
      , output = []
      }
  , -- Dropping any number of elements from an empty list always succeeds (and
    -- returns the empty list).
    Params
      { count = 1234
      , input = []
      , output = []
      }
  ]

-- Failing inputs for lists of UPLC integers.
---These should return the empty list.  However, if run in restricting mode they
-- will (probably, depending on the cost model) attempt to consume the maxmimum
-- budget and fail because the cost depends on the absolute value of the `count`
-- argument, irrespective of the size of the list.
expensiveDropListParams :: [Params]
expensiveDropListParams =
  [ Params
      { count = 10000000000000000000
      , input = [11, 22, 33, 44, 55, 66, 77, 88, 99]
      , output = []
      }
  , Params
      { count = -12345678901234567890
      , input = [11, 22, 33, 44, 55, 66, 77, 88, 99]
      , output = []
      }
  , Params -- Drop (maxBound::Int)-1 elements
      { count = 9223372036854775806
      , input =
          [ 20
          , 21
          , 22
          , 23
          , 24
          , 25
          , 26
          , 27
          , 28
          , 29
          , 30
          , 31
          , 32
          , 33
          , 34
          , 35
          , 36
          , 37
          , 38
          , 39
          , 40
          , 41
          , 42
          , 43
          , 44
          , 45
          , 46
          , 47
          , 48
          , 49
          , 50
          , 51
          , 52
          , 53
          , 54
          , 55
          , 56
          , 57
          , 58
          , 59
          , 60
          , 61
          , 62
          , 63
          , 64
          , 65
          , 66
          , 67
          , 68
          , 69
          , 70
          , 71
          , 72
          , 73
          , 74
          , 75
          , 76
          , 77
          , 78
          , 79
          , 80
          , 81
          , 82
          , 83
          , 84
          , 85
          , 86
          , 87
          , 88
          , 89
          , 90
          , 91
          , 92
          , 93
          , 94
          , 95
          , 96
          , 97
          , 98
          , 99
          , 100
          , 101
          , 102
          , 103
          , 104
          , 105
          , 106
          , 107
          , 108
          , 109
          , 110
          , 111
          ]
      , output = []
      }
  , Params -- Drop (maxBound::Int) elements
      { count = 9223372036854775807
      , input =
          [ 20
          , 21
          , 22
          , 23
          , 24
          , 25
          , 26
          , 27
          , 28
          , 29
          , 30
          , 31
          , 32
          , 33
          , 34
          , 35
          , 36
          , 37
          , 38
          , 39
          , 40
          , 41
          , 42
          , 43
          , 44
          , 45
          , 46
          , 47
          , 48
          , 49
          , 50
          , 51
          , 52
          , 53
          , 54
          , 55
          , 56
          , 57
          , 58
          , 59
          , 60
          , 61
          , 62
          , 63
          , 64
          , 65
          , 66
          , 67
          , 68
          , 69
          , 70
          , 71
          , 72
          , 73
          , 74
          , 75
          , 76
          , 77
          , 78
          , 79
          , 80
          , 81
          , 82
          , 83
          , 84
          , 85
          , 86
          , 87
          , 88
          , 89
          , 90
          , 91
          , 92
          , 93
          , 94
          , 95
          , 96
          , 97
          , 98
          , 99
          , 100
          , 101
          , 102
          , 103
          , 104
          , 105
          , 106
          , 107
          , 108
          , 109
          , 110
          , 111
          ]
      , output = []
      }
  , Params -- Drop (maxBound::Int)+1 elements
      { count = 9223372036854775808
      , input =
          [ 20
          , 21
          , 22
          , 23
          , 24
          , 25
          , 26
          , 27
          , 28
          , 29
          , 30
          , 31
          , 32
          , 33
          , 34
          , 35
          , 36
          , 37
          , 38
          , 39
          , 40
          , 41
          , 42
          , 43
          , 44
          , 45
          , 46
          , 47
          , 48
          , 49
          , 50
          , 51
          , 52
          , 53
          , 54
          , 55
          , 56
          , 57
          , 58
          , 59
          , 60
          , 61
          , 62
          , 63
          , 64
          , 65
          , 66
          , 67
          , 68
          , 69
          , 70
          , 71
          , 72
          , 73
          , 74
          , 75
          , 76
          , 77
          , 78
          , 79
          , 80
          , 81
          , 82
          , 83
          , 84
          , 85
          , 86
          , 87
          , 88
          , 89
          , 90
          , 91
          , 92
          , 93
          , 94
          , 95
          , 96
          , 97
          , 98
          , 99
          , 100
          , 101
          , 102
          , 103
          , 104
          , 105
          , 106
          , 107
          , 108
          , 109
          , 110
          , 111
          ]
      , output = []
      }
  ]
