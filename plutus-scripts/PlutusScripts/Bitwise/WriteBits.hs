{- | Simple end-to-end tests for the Plutus Core `writeBits` builtin.  These are
adapted from the `plutus-conformance` tests.
-}
module PlutusScripts.Bitwise.WriteBits where

import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)
import PlutusTx.Prelude qualified as P

import PlutusScripts.Helpers (hxs)

data Params = Params
  { s :: P.BuiltinByteString
  , is :: [Integer]
  , b :: Bool
  , output :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

{-# INLINEABLE mkWriteBitsPolicy #-}
mkWriteBitsPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkWriteBitsPolicy l _ctx = go l
 where
  go [] = BI.unitval
  go (Params{..} : rest) =
    if BI.writeBits s is b P.== output
      then go rest
      else P.traceError "mkWriteBitsPolicy"

succeedingWriteBitsParams :: [Params]
succeedingWriteBitsParams =
  [ Params
      { s = hxs "ff"
      , is = [0]
      , b = False
      , output = hxs "fe"
      }
  , Params
      { s = hxs "ff"
      , is = [1]
      , b = False
      , output = hxs "fd"
      }
  , Params
      { s = hxs "ff"
      , is = [2]
      , b = False
      , output = hxs "fb"
      }
  , Params
      { s = hxs "ff"
      , is = [3]
      , b = False
      , output = hxs "f7"
      }
  , Params
      { s = hxs "ff"
      , is = [4]
      , b = False
      , output = hxs "ef"
      }
  , Params
      { s = hxs "ff"
      , is = [5]
      , b = False
      , output = hxs "df"
      }
  , Params
      { s = hxs "ff"
      , is = [6]
      , b = False
      , output = hxs "bf"
      }
  , Params
      { s = hxs "ff"
      , is = [7]
      , b = False
      , output = hxs "7f"
      }
  , Params
      { s = hxs "00"
      , is = [0]
      , b = True
      , output = hxs "01"
      }
  , Params
      { s = hxs "00"
      , is = [1]
      , b = True
      , output = hxs "02"
      }
  , Params
      { s = hxs "00"
      , is = [2]
      , b = True
      , output = hxs "04"
      }
  , Params
      { s = hxs "00"
      , is = [3]
      , b = True
      , output = hxs "08"
      }
  , Params
      { s = hxs "00"
      , is = [4]
      , b = True
      , output = hxs "10"
      }
  , Params
      { s = hxs "00"
      , is = [5]
      , b = True
      , output = hxs "20"
      }
  , Params
      { s = hxs "00"
      , is = [6]
      , b = True
      , output = hxs "40"
      }
  , Params
      { s = hxs "00"
      , is = [7]
      , b = True
      , output = hxs "80"
      }
  , Params
      { s = hxs "f4ff"
      , is = [10]
      , b = False
      , output = hxs "f0ff"
      }
  , Params
      { s = hxs "f4ff"
      , is = [1, 10]
      , b = False
      , output = hxs "f0fd"
      }
  , Params
      { s = hxs "f4ff"
      , is = [10, 1]
      , b = False
      , output = hxs "f0fd"
      }
  , Params
      { s = hxs "f4ff"
      , is = [10, 1, 10, 1, 1, 1, 1, 10, 10, 10, 1, 1, 1, 1, 1, 1, 10, 1]
      , b = False
      , output = hxs "f0fd"
      }
  , Params
      { s = hxs "f4ff"
      , is = [1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 1, 10, 10, 10, 10, 11, 11, 9]
      , b = False
      , output = hxs "f0fd"
      }
  , Params
      { s = hxs "ff"
      , is = [0]
      , b = True
      , output = hxs "ff"
      }
  , Params
      { s = hxs "00"
      , is = [0]
      , b = False
      , output = hxs "00"
      }
  , Params
      { s = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      , is = [340, 342, 343]
      , b = True
      , output =
          hxs "d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      }
  , Params
      { s = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      , is = [340, 342, 343]
      , b = False
      , output =
          hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      }
  , Params
      { s = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      , is = [340, 342, 343, 343, 342, 340, 340, 343, 342, 340, 340, 340]
      , b = True
      , output =
          hxs "d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      }
  ]

failingWriteBitsParams :: [Params]
failingWriteBitsParams =
  [ Params
      { s = hxs ""
      , is = [0]
      , b = False
      , output = hxs ""
      }
  , Params
      { s = hxs ""
      , is = [15]
      , b = False
      , output = hxs ""
      }
  , Params
      { s = hxs ""
      , is = [0]
      , b = True
      , output = hxs ""
      }
  , Params
      { s = hxs ""
      , is = [0, 1]
      , b = False
      , output = hxs ""
      }
  , Params
      { s = hxs "ff"
      , is = [-1]
      , b = False
      , output = hxs ""
      }
  , Params
      { s = hxs "ff"
      , is = [0, -1]
      , b = False
      , output = hxs ""
      }
  , Params
      { s = hxs "ff"
      , is = [-1, 0]
      , b = False
      , output = hxs ""
      }
  , Params
      { s = hxs "ff"
      , is = [8]
      , b = False
      , output = hxs ""
      }
  , Params
      { s = hxs "ff"
      , is = [1, 8]
      , b = False
      , output = hxs ""
      }
  , Params
      { s = hxs "ff"
      , is = [8, 1]
      , b = False
      , output = hxs ""
      }
  , Params
      { s = hxs "00"
      , is = [-1]
      , b = True
      , output = hxs ""
      }
  , Params
      { s = hxs "00"
      , is = [8]
      , b = True
      , output = hxs ""
      }
  , Params
      { s = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      , is = [340, 342, 344]
      , b = True
      , output = hxs ""
      }
  , Params
      { s = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
      , is = [340, 342, 1000000]
      , b = True
      , output = hxs ""
      }
  , Params
      { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
      , is = [9223372036854775807] -- maxBound :: Int
      , b = True
      , output = hxs ""
      }
  , Params
      { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
      , is = [9223372036854775808] -- (maxBound :: Int) + 1
      , b = True
      , output = hxs ""
      }
  , Params
      { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
      , is = [-9223372036854775808] -- minBound :: Int
      , b = True
      , output = hxs ""
      }
  , Params
      { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
      , is = [-9223372036854775809] -- (minBound :: Int) - 1
      , b = True
      , output = hxs ""
      }
  , Params
      { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
      , is = [18446744073709551616] -- 2^64
      , b = True
      , output = hxs ""
      }
  ]
