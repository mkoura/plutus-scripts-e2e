-- subset of utilities from plutus-script-utils
module Helpers.ScriptUtils where

import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx (UnsafeFromData)
import PlutusTx.Builtins.Internal qualified as BI (
  BuiltinList,
  snd,
  unsafeDataAsConstr,
 )
import PlutusTx.Prelude qualified as P

type UntypedValidator = V1.BuiltinData -> V1.BuiltinData -> V1.BuiltinData -> ()
type UntypedMintingPolicy = V1.BuiltinData -> V1.BuiltinData -> ()
type UntypedStakeValidator = V1.BuiltinData -> V1.BuiltinData -> ()

{-# INLINEABLE tracedUnsafeFrom #-}
tracedUnsafeFrom :: forall a. (UnsafeFromData a) => P.BuiltinString -> V1.BuiltinData -> a
tracedUnsafeFrom label d = P.trace label $ V1.unsafeFromBuiltinData d

class (V1.UnsafeFromData sc) => IsScriptContext sc where
  {-# INLINEABLE mkUntypedValidator #-}
  mkUntypedValidator
    :: (V1.UnsafeFromData d, V1.UnsafeFromData r)
    => (d -> r -> sc -> Bool)
    -> UntypedValidator
  -- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
  mkUntypedValidator f d r p =
    check $
      f
        (tracedUnsafeFrom "Data decoded successfully" d)
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

  {-# INLINEABLE mkUntypedStakeValidator #-}
  mkUntypedStakeValidator
    :: (V1.UnsafeFromData r)
    => (r -> sc -> Bool)
    -> UntypedStakeValidator
  mkUntypedStakeValidator f r p =
    check $
      f
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

  {-# INLINEABLE mkUntypedMintingPolicy #-}
  mkUntypedMintingPolicy
    :: (UnsafeFromData r)
    => (r -> sc -> Bool)
    -> V3.BuiltinData
    -> V3.BuiltinData
    -> P.BuiltinUnit
  mkUntypedMintingPolicy f r p =
    P.check $
      f
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

type ScriptContextV1 = V1.ScriptContext
type ScriptContextV2 = V2.ScriptContext
type ScriptContextV3 = V3.ScriptContext

instance IsScriptContext V1.ScriptContext
instance IsScriptContext V2.ScriptContext
instance IsScriptContext V3.ScriptContext

{-# INLINEABLE check #-}

-- | Checks a 'Bool' and aborts if it is false.
check :: Bool -> ()
check b = if b then () else P.traceError "PT5"

{-# INLINEABLE constrArgs #-}
constrArgs :: P.BuiltinData -> BI.BuiltinList P.BuiltinData
constrArgs = BI.snd . BI.unsafeDataAsConstr
