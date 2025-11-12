{- | Simple end-to-end tests for the Plutus Core `ripemd_160` builtin.
These are adapted from the `plutus-conformance` tests.
-}
module PlutusScripts.Hashing.Ripemd_160 (mkRipemd_160Policy, succeedingRipemd_160Params)
where

import PlutusScripts.Helpers (hxs)

import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)
import PlutusTx.Prelude qualified as P

data Params = Params
    { input :: P.BuiltinByteString
    , output :: P.BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

{-# INLINEABLE mkRipemd_160Policy #-}
mkRipemd_160Policy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkRipemd_160Policy l _ctx = go l
  where
    go [] = BI.unitval
    go (Params{..} : rest) =
        if BI.ripemd_160 input P.== output
            then go rest
            else P.traceError "mkRIPEMD_160Policy"

-- Succeeding inputs; `ripemd_160` can't fail.
succeedingRipemd_160Params :: [Params]
succeedingRipemd_160Params =
    [ Params
        { input = hxs ""
        , output = hxs "9c1185a5c5e9fc54612808977ee8f548b2258d31"
        }
    , Params
        { input = hxs "2e7ea84da4bc4d7cfb463e3f2c8647057afff3fbececa1d200"
        , output = hxs "f18921115370b049e99dfdd49fc92b371dd7c7e9"
        }
    ]
