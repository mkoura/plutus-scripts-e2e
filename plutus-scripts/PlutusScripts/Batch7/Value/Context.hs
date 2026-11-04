-- | Spending wrappers decode each ledger language's own context representation.
module PlutusScripts.Batch7.Value.Context (mkStateV3, mkStateV4) where

import PlutusLedgerApi.V3 qualified as V3
import PlutusLedgerApi.V3.Contexts qualified as V3
import PlutusLedgerApi.V4 qualified as V4
import PlutusLedgerApi.V4.Contexts qualified as V4
import PlutusScripts.Batch7.Value.Common (checkTransition)
import PlutusTx.Builtins qualified as B
import PlutusTx.Builtins.Internal qualified as BI (unitval)
import PlutusTx.List qualified as List
import PlutusTx.Prelude qualified as P

-- | The redeemer is Constr 0 [mode, protocol policy bytes, expected count].
{-# INLINEABLE redeemerFields #-}
redeemerFields :: P.BuiltinData -> (Integer, P.BuiltinByteString, Integer)
redeemerFields raw =
  let (tag, fields) = B.unsafeDataAsConstr raw
   in case fields of
        [mode, protocol, expected] ->
          if tag P.== 0
            then
              ( P.unsafeFromBuiltinData mode
              , P.unsafeFromBuiltinData protocol
              , P.unsafeFromBuiltinData expected
              )
            else P.traceError "value state redeemer"
        _ -> P.traceError "value state redeemer"

{-# INLINEABLE mkStateV3 #-}
mkStateV3 :: P.BuiltinData -> P.BuiltinUnit
mkStateV3 raw =
  let ctx = P.unsafeFromBuiltinData raw :: V3.ScriptContext
      info = V3.scriptContextTxInfo ctx
      (mode, protocol, expected) =
        redeemerFields
          (V3.getRedeemer (V3.scriptContextRedeemer ctx))
   in case V3.findOwnInput ctx of
        P.Nothing -> P.traceError "value state own input"
        P.Just own ->
          let input = V3.txInInfoResolved own
              outputs =
                List.filter
                  (\o -> V3.txOutAddress o P.== V3.txOutAddress input)
                  (V3.txInfoOutputs info)
           in case outputs of
                [output] ->
                  if List.length
                    ( List.filter
                        ( \i ->
                            V3.txOutAddress (V3.txInInfoResolved i)
                              P.== V3.txOutAddress input
                        )
                        (V3.txInfoInputs info)
                    )
                    P.== 1
                    P.&& checkTransition
                      mode
                      protocol
                      expected
                      (B.unsafeDataAsValue (P.toBuiltinData (V3.txOutValue input)))
                      (B.unsafeDataAsValue (P.toBuiltinData (V3.txOutValue output)))
                    then BI.unitval
                    else P.traceError "value state transition"
                _ -> P.traceError "value state continuing output"

{-# INLINEABLE mkStateV4 #-}
mkStateV4 :: P.BuiltinData -> P.BuiltinUnit
mkStateV4 raw =
  let ctx = P.unsafeFromBuiltinData raw :: V4.ScriptContext
      info = V4.scriptContextTxInfo ctx
      (mode, protocol, expected) =
        redeemerFields
          (V4.getRedeemer (V4.scriptContextRedeemer ctx))
   in case V4.findOwnInput ctx of
        P.Nothing -> P.traceError "value state own input"
        P.Just own ->
          let input = V4.txInInfoResolved own
              outputs =
                List.filter
                  (\o -> V4.txOutAddress o P.== V4.txOutAddress input)
                  (V4.txInfoOutputs info)
           in case outputs of
                [output] ->
                  if List.length
                    ( List.filter
                        ( \i ->
                            V4.txOutAddress (V4.txInInfoResolved i)
                              P.== V4.txOutAddress input
                        )
                        (V4.txInfoInputs info)
                    )
                    P.== 1
                    P.&& checkTransition
                      mode
                      protocol
                      expected
                      (B.unsafeDataAsValue (P.toBuiltinData (V4.txOutValue input)))
                      (B.unsafeDataAsValue (P.toBuiltinData (V4.txOutValue output)))
                    then BI.unitval
                    else P.traceError "value state transition"
                _ -> P.traceError "value state continuing output"
