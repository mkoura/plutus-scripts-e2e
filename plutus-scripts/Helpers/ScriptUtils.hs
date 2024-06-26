{-# LANGUAGE OverloadedStrings #-}

-- subset of utilities from plutus-script-utils
module Helpers.ScriptUtils where

import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V3 qualified as PV3
import PlutusTx (UnsafeFromData)
import PlutusTx.Builtins.Internal qualified as BI (
  BuiltinList,
  snd,
  unsafeDataAsConstr,
 )
import PlutusTx.Prelude qualified as P

type UntypedValidator = PV1.BuiltinData -> PV1.BuiltinData -> PV1.BuiltinData -> ()
type UntypedMintingPolicy = PV1.BuiltinData -> PV1.BuiltinData -> ()
type UntypedStakeValidator = PV1.BuiltinData -> PV1.BuiltinData -> ()

{-# INLINEABLE tracedUnsafeFrom #-}
tracedUnsafeFrom :: forall a. (UnsafeFromData a) => P.BuiltinString -> PV1.BuiltinData -> a
tracedUnsafeFrom label d = P.trace label $ PV1.unsafeFromBuiltinData d

class (PV1.UnsafeFromData sc) => IsScriptContext sc where
  {-# INLINEABLE mkUntypedValidator #-}
  mkUntypedValidator ::
    (PV1.UnsafeFromData d, PV1.UnsafeFromData r) =>
    (d -> r -> sc -> Bool) ->
    UntypedValidator
  -- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
  mkUntypedValidator f d r p =
    check $
      f
        (tracedUnsafeFrom "Data decoded successfully" d)
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

  {-# INLINEABLE mkUntypedStakeValidator #-}
  mkUntypedStakeValidator ::
    (PV1.UnsafeFromData r) =>
    (r -> sc -> Bool) ->
    UntypedStakeValidator
  mkUntypedStakeValidator f r p =
    check $
      f
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

  {-# INLINEABLE mkUntypedMintingPolicy #-}
  mkUntypedMintingPolicy ::
    (UnsafeFromData r) =>
    (r -> sc -> Bool) ->
    PV3.BuiltinData ->
    PV3.BuiltinData ->
    P.BuiltinUnit
  mkUntypedMintingPolicy f r p =
    P.check $
      f
        (tracedUnsafeFrom "Redeemer decoded successfully" r)
        (tracedUnsafeFrom "Script context decoded successfully" p)

type ScriptContextV1 = PV1.ScriptContext
type ScriptContextV2 = PV2.ScriptContext
type ScriptContextV3 = PV3.ScriptContext

instance IsScriptContext PV1.ScriptContext
instance IsScriptContext PV2.ScriptContext
instance IsScriptContext PV3.ScriptContext

{-# INLINEABLE check #-}

-- | Checks a 'Bool' and aborts if it is false.
check :: Bool -> ()
check b = if b then () else P.traceError "PT5"

{-# INLINEABLE constrArgs #-}
constrArgs :: P.BuiltinData -> BI.BuiltinList P.BuiltinData
constrArgs = BI.snd . BI.unsafeDataAsConstr
