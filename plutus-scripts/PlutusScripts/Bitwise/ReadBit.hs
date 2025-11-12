{- | Simple end-to-end tests for the Plutus Core `readBit`builtin.  These are
adapted from the `plutus-conformance` tests.
-}
module PlutusScripts.Bitwise.ReadBit where

import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)
import PlutusTx.Prelude qualified as P

import PlutusScripts.Helpers (hxs)

data Params = Params
    { s :: P.BuiltinByteString
    , i :: Integer
    , output :: Bool
    }
PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

{-# INLINEABLE mkReadBitPolicy #-}
mkReadBitPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkReadBitPolicy l _ctx = go l
  where
    go [] = BI.unitval
    go (Params{..} : rest) =
        if BI.readBit s i P.== output
            then go rest
            else P.traceError "mkReadBitPolicy"

succeedingReadBitParams :: [Params]
succeedingReadBitParams =
    [ Params
        { s = hxs "f4"
        , i = 0
        , output = False
        }
    , Params
        { s = hxs "f4"
        , i = 1
        , output = False
        }
    , Params
        { s = hxs "f4"
        , i = 2
        , output = True
        }
    , Params
        { s = hxs "f4"
        , i = 3
        , output = False
        }
    , Params
        { s = hxs "f4"
        , i = 4
        , output = True
        }
    , Params
        { s = hxs "f4"
        , i = 5
        , output = True
        }
    , Params
        { s = hxs "f4"
        , i = 6
        , output = True
        }
    , Params
        { s = hxs "f4"
        , i = 7
        , output = True
        }
    , Params
        { s = hxs "f4ff"
        , i = 10
        , output = True
        }
    , Params
        { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
        , i = 0
        , output = False
        }
    , Params
        { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
        , i = 341 -- Third bit from left
        , output = True
        }
    , Params
        { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
        , i = 343 -- Leftmost bit
        , output = False
        }
    ]

failingReadBitParams :: [Params]
failingReadBitParams =
    [ Params
        { s = hxs ""
        , i = 0
        , output = False
        }
    , Params
        { s = hxs ""
        , i = 345
        , output = False
        }
    , Params
        { s = hxs ""
        , i = -1
        , output = False
        }
    , Params
        { s = hxs "ff"
        , i = -1
        , output = False
        }
    , Params
        { s = hxs "ff"
        , i = -1
        , output = False
        }
    , Params
        { s = hxs "ff"
        , i = -1
        , output = False
        }
    , Params
        { s = hxs "f4"
        , i = 6
        , output = False
        }
    , Params
        { s = hxs "fff4"
        , i = 16
        , output = False
        }
    , Params
        { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
        , i = 344 -- Just past the end
        , output = False
        }
    , Params
        { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
        , i = 344 -- Just past the end
        , output = False
        }
    , Params
        { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
        , i = 9223372036854775807 -- maxBound :: Int64
        , output = False
        }
    , Params
        { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
        , i = 9223372036854775808 -- (maxBound :: Int64) + 1
        , output = False
        }
    , Params
        { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
        , i = -9223372036854775808 -- minBound :: Int64
        , output = False
        }
    , Params
        { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
        , i = -9223372036854775809 -- (minBound :: Int64) - 1
        , output = False
        }
    ]
