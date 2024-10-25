{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module PlutusScripts.Bitwise.WriteBits where

import PlutusTx.Prelude qualified as P
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)

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
  where go [] = BI.unitval
        go (Params{..}:rest) =
          if BI.writeBits s is b P.== output
          then go rest
          else P.traceError "mkWriteBitsPolicy"

-- INCOMPLETE
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
    , output = hxs "ef"
    }
  , Params
    { s = hxs "ff"
    , is = [7]
    , b = False
    , output = hxs "bf"
    }
  , Params
    { s = hxs "00"
    , is = [5]
    , b = True
    , output = hxs "20"
    }
  , Params
    { s = hxs "ff"
    , is = [5]
    , b = False
    , output = hxs "df"
    }
  , Params
    { s = hxs "f4ff"
    , is = [10]
    , b = False
    , output = hxs "f0ff"
    }
  , Params
    { s = hxs "f4ff"
    , is = [10,1]
    , b = False
    , output = hxs "f0fd"
    }
  , Params
    { s = hxs "f4ff"
    , is = [10,1]
    , b = False
    , output = hxs "f0ff"
    }
  ]

-- INCOMPLETE
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
    , is = [15]
    , b = False
    , output = hxs ""
    }
  , Params
    { s = hxs ""
    , is = [0,1]
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
    , is = [0,-1]
    , b = False
    , output = hxs ""
    }
  , Params
    { s = hxs "ff"
    , is = [-1,0]
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
    , is = [1,8]
    , b = False
    , output = hxs ""
    }
  , Params
    { s = hxs "ff"
    , is = [8,1]
    , b = False
    , output = hxs ""
    }
  ]
