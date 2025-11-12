{-| Simple end-to-end tests for the Plutus Core `replicateByte` builtin.  These
are adapted from the `plutus-conformance` tests. -}


module PlutusScripts.Bitwise.ReplicateByte (
  mkReplicateBytePolicy,
  failingReplicateByteParams,
  succeedingReplicateByteParams)
where

import PlutusTx.Prelude qualified as P
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)

import Data.ByteString qualified as BS (pack)
import PlutusScripts.Helpers (hxs)

data Params = Params
  { count :: Integer
  , byte :: Integer
  , output :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

{-# INLINEABLE mkReplicateBytePolicy #-}
mkReplicateBytePolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkReplicateBytePolicy l _ctx = go l
  where go [] = BI.unitval
        go (Params{..}:rest) =
          if BI.replicateByte count byte P.== output
          then go rest
          else P.traceError "mkReplicateBytePolicy"

succeedingReplicateByteParams :: [Params]
succeedingReplicateByteParams =
  [ Params
    { count = 0
    , byte = 255
    , output = hxs ""
    }
  , Params
    { count = 100
    , byte = 0x23
    , output = hxs "23232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323"
    }
  , Params
    { count = 8192  -- Maximum allowed
    , byte = 141
    , output = BI.toBuiltin $ BS.pack (replicate 8192 141)
    }
  ]

failingReplicateByteParams :: [Params]
failingReplicateByteParams =
  [ Params
    { count = -1
    , byte = 0
    , output = hxs ""
    }
  , Params
    { count = -1
    , byte = 3
    , output = hxs ""
    }
  , Params
    { count = 1
    , byte = -1
    , output = hxs ""
    }
  , Params
    { count = 4
    , byte = -1
    , output = hxs ""
    }
  , Params
    { count = 4
    , byte = 256
    , output = hxs ""
    }
  , Params
    { count = 8193  -- Maximum size is 8192
    , byte = 141
    , output = hxs ""
    }
  ]
