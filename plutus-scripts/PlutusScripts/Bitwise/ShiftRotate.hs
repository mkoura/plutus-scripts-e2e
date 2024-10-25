{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module PlutusScripts.Bitwise.ShiftRotate (
  mkRotateByteStringPolicy,
  mkShiftByteStringPolicy,
  succeedingRotateByteStringParams,
  succeedingShiftByteStringParams
  )
where

import PlutusTx.Prelude qualified as P
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)

import PlutusScripts.Helpers (hxs)

data Params = Params
  { s :: P.BuiltinByteString
  , n :: Integer
  , output :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

{-# INLINEABLE mkShiftByteStringPolicy #-}
mkShiftByteStringPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkShiftByteStringPolicy l _ctx = go l
  where go [] = BI.unitval
        go (Params{..}:rest) =
          if BI.shiftByteString s n P.== output
          then go rest
          else P.traceError "mkShiftByteStringPolicy"

{-# INLINEABLE mkRotateByteStringPolicy #-}
mkRotateByteStringPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkRotateByteStringPolicy l _ctx = go l
  where go [] = BI.unitval
        go (Params{..}:rest) =
          if BI.rotateByteString s n P.== output
          then go rest
          else P.traceError "mkRotateByteStringPolicy"

-- Succeeding inputs; `shiftByteString` can't fail.
succeedingShiftByteStringParams :: [Params]
succeedingShiftByteStringParams =
  [ Params
    { s = hxs ""
    , n = 3
    , output = hxs ""
    }
  , Params
    { s = hxs ""
    , n = -3
    , output = hxs ""
    }
  , Params
    { s = hxs "ebfc"
    , n = 5
    , output = hxs "7f80"
    }
  , Params
    { s = hxs "ebfc"
    , n = -5
    , output = hxs "075f"
    }
  , Params
    { s = hxs "ebfc"
    , n = 16
    , output = hxs "0000"
    }
  , Params
    { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , n = 0
    , output = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    }
  , Params
    { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , n = 23
    , output = hxs "6b42c2401dcee02d9d85ce5eb341f6e0673a15d84cabb09f220a8102b3ce9fb0d233d92e63ac51d8000000"
    }
  , Params
    { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , n = -23
    , output = hxs "0000006785e1ad0b0900773b80b67617397acd07db819ce8576132aec27c882a040acf3a7ec348cf64b98e"
    }
  , Params
    { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , n = 1000000
    , output = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    }
  , Params
    { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , n = -1000000
    , output = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    }
  , Params
    { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , n = 9223372036854775807  -- maxBound :: Int64
    , output = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    }
  , Params
    { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , n = 9223372036854775808  -- (maxBound :: Int64) + 1
    , output = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    }
  , Params
    { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , n = -9223372036854775808  -- minBound :: Int64
    , output = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    }
  , Params
    { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , n =  -9223372036854775809  -- (minBound :: Int64) - 1
    , output = hxs "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    }
 ]

-- Succeeding inputs; `rotateByteString` can't fail.
succeedingRotateByteStringParams :: [Params]
succeedingRotateByteStringParams =
  [  Params
     { s = hxs ""
     , n = 3
     , output = hxs ""
     },
     Params
     { s = hxs ""
     , n = -1
     , output = hxs ""
     },
     Params
     { s = hxs "ebfc"
     , n = 5
     , output = hxs "7f9d"
     },
     Params
     { s = hxs "ebfc"
     , n = -5
     , output = hxs "e75f"
     },
     Params
     { s = hxs "ebfc"
     , n = 16
     , output = hxs "ebfc"
     },
     Params
     { s = hxs "ebfc"
     , n = -16
     , output = hxs "ebfc"
     },
     Params
     { s = hxs "ebfc"
     , n = 21
     , output = hxs "7f9d"
     },
     Params
     { s = hxs "ebfc"
     , n = -21
     , output = hxs "e75f"
     },
     Params
     { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     , n = 0
     , output = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     },
     Params
     { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     , n = 344000  -- 1000 times the bit length
     , output = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     },
     Params
     { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     , n = -344000  -- -1000 times the bit length
     , output = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     },
     Params
     { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     , n = 23
     , output = hxs "6b42c2401dcee02d9d85ce5eb341f6e0673a15d84cabb09f220a8102b3ce9fb0d233d92e63ac51d819e178"
     },
     Params
     { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     , n = 344023
     , output = hxs "6b42c2401dcee02d9d85ce5eb341f6e0673a15d84cabb09f220a8102b3ce9fb0d233d92e63ac51d819e178"
     },
     Params
     { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     , n = -3439977
     , output = hxs "6b42c2401dcee02d9d85ce5eb341f6e0673a15d84cabb09f220a8102b3ce9fb0d233d92e63ac51d819e178"
     },
     Params
     { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     , n = 9223372036854775807  -- maxBound :: Int64
     , output = hxs "41f6e0673a15d84cabb09f220a8102b3ce9fb0d233d92e63ac51d819e1786b42c2401dcee02d9d85ce5eb3"
     },
     Params
     { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     , n = 9223372036854775808  -- (maxBound :: Int64) + 1
     , output = hxs "83edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b033c2f0d68584803b9dc05b3b0b9cbd66"
     },
     Params
     { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     , n = -9223372036854775808  -- minBound :: Int64
     , output = hxs "44150205679d3f61a467b25cc758a3b033c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e"
     },
     Params
     { s = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
     , n = -9223372036854775809  -- (minBound :: Int64) - 1
     , output = hxs "220a8102b3ce9fb0d233d92e63ac51d819e1786b42c2401dcee02d9d85ce5eb341f6e0673a15d84cabb09f"
     }
  ]
