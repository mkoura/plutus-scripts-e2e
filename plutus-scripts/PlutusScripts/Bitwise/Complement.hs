{- | Simple end-to-end tests for the Plutus Core `complementByteString` builtin.
These are adapted from the `plutus-conformance` tests.
-}
module PlutusScripts.Bitwise.Complement (mkComplementByteStringPolicy, succeedingComplementByteStringParams)
where

import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)
import PlutusTx.Prelude qualified as P

import PlutusScripts.Helpers (hxs)

data Params = Params
  { input :: P.BuiltinByteString
  , output :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

{-# INLINEABLE mkComplementByteStringPolicy #-}
mkComplementByteStringPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkComplementByteStringPolicy l _ctx = go l
 where
  go [] = BI.unitval
  go (Params{..} : rest) =
    if BI.complementByteString input P.== output
      then go rest
      else P.traceError "mkComplementByteStringPolicy"

-- Succeeding inputs; `complementByteString` can't fail.
succeedingComplementByteStringParams :: [Params]
succeedingComplementByteStringParams =
  [ Params
      { input = hxs ""
      , output = hxs ""
      }
  , Params
      { input = hxs "0f"
      , output = hxs "f0"
      }
  , Params
      { input = hxs "b00b"
      , output = hxs "4ff4"
      }
  , Params
      { input =
          hxs
            "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
      , output =
          hxs
            "246379e3675c2e6346d73dd5cd5551f5b08bfeec23b78cb2c3ffe9a834702d46b68050e95bf3e1322829a7e4aa49daaac50c"
      }
  , Params
      { input =
          hxs
            "246379e3675c2e6346d73dd5cd5551f5b08bfeec23b78cb2c3ffe9a834702d46b68050e95bf3e1322829a7e4aa49daaac50c"
      , output =
          hxs
            "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
      }
  ]
