{- | Simple end-to-end tests for the Plutus Core `countSetBits` and
`findFirstSetBit` builtins.  These are adapted from the `plutus-conformance`
tests.
-}
module PlutusScripts.Bitwise.CountFindFirstSet (
    mkCountSetBitsPolicy,
    mkFindFirstSetBitPolicy,
    succeedingCountSetBitsParams,
    succeedingFindFirstSetBitParams,
)
where

import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)
import PlutusTx.Prelude qualified as P

import PlutusScripts.Helpers (hxs)

data Params = Params
    { input :: P.BuiltinByteString
    , output :: Integer
    }
PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

{-# INLINEABLE mkCountSetBitsPolicy #-}
mkCountSetBitsPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkCountSetBitsPolicy l _ctx = go l
  where
    go [] = BI.unitval
    go (Params{..} : rest) =
        if BI.countSetBits input P.== output
            then go rest
            else P.traceError "mkCountSetBitsPolicy"

{-# INLINEABLE mkFindFirstSetBitPolicy #-}
mkFindFirstSetBitPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkFindFirstSetBitPolicy l _ctx = go l
  where
    go [] = BI.unitval
    go (Params{..} : rest) =
        if BI.findFirstSetBit input P.== output
            then go rest
            else P.traceError "mkFindFirstSetBitPolicy"

-- Succeeding inputs; `countSetBits` can't fail.
succeedingCountSetBitsParams :: [Params]
succeedingCountSetBitsParams =
    [ Params
        { input = hxs ""
        , output = 0
        }
    , Params
        { input = hxs "0000"
        , output = 0
        }
    , Params
        { input = hxs "0100"
        , output = 1
        }
    , Params
        { input = hxs "0001"
        , output = 1
        }
    , Params
        { input = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
        , output = 163
        }
    , Params
        { input = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        , output = 0
        }
    , Params
        { input = hxs "00000000000000100000000000000000000000000000000000000000000000000000000000000000000000"
        , output = 1
        }
    , Params
        { input = hxs "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        , output = 344
        }
    ]

-- Succeeding inputs; `findFirstSetBit` can't fail.
succeedingFindFirstSetBitParams :: [Params]
succeedingFindFirstSetBitParams =
    [ Params
        { input = hxs ""
        , output = -1
        }
    , Params
        { input = hxs "0000"
        , output = -1
        }
    , Params
        { input = hxs "0002"
        , output = 1
        }
    , Params
        { input = hxs "fff2"
        , output = 1
        }
    , Params
        { input = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        , output = -1
        }
    , Params
        { input = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000001"
        , output = 0
        }
    , Params
        { input = hxs "50000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
        , output = 340
        }
    ]
