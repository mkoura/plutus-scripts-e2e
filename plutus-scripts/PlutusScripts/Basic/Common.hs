module PlutusScripts.Basic.Common where

import Helpers.ScriptUtils (check, constrArgs)
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V3 qualified as V3
import PlutusLedgerApi.V4 qualified as V4
import PlutusLedgerApi.V4.Contexts qualified as V4Contexts
import PlutusTx.Builtins.Internal qualified as BI (
  BuiltinList,
  head,
  tail,
  unitval,
 )
import PlutusTx.List qualified as List
import PlutusTx.Prelude qualified as P

-- AlwaysSucceeds minting policy --

{-# INLINEABLE mkAlwaysSucceedPolicyV3 #-}
mkAlwaysSucceedPolicyV3 :: P.BuiltinData -> P.BuiltinUnit
mkAlwaysSucceedPolicyV3 _sc = BI.unitval

{-# INLINEABLE mkAlwaysSucceedPolicy #-}
mkAlwaysSucceedPolicy :: P.BuiltinData -> P.BuiltinData -> ()
mkAlwaysSucceedPolicy _datum _sc = ()

-- AlwaysSucceeds validator --

{-# INLINEABLE mkAlwaysSucceedSpend #-}
mkAlwaysSucceedSpend :: P.BuiltinData -> P.BuiltinData -> P.BuiltinData -> ()
mkAlwaysSucceedSpend _datum _redeemer _sc = ()

-- AlwaysFails minting policy --

{-# INLINEABLE mkAlwaysFailsPolicy #-}
mkAlwaysFailsPolicy :: P.BuiltinData -> P.BuiltinData -> ()
mkAlwaysFailsPolicy _datum _sc = check $ P.error ()

{-# INLINEABLE mkAlwaysFailsPolicyV3 #-}
mkAlwaysFailsPolicyV3 :: P.BuiltinData -> P.BuiltinUnit
mkAlwaysFailsPolicyV3 _sc = P.check $ P.error ()

-- Mint token name policy --

{-# INLINEABLE mkMintTokenNamePolicyV3 #-}
mkMintTokenNamePolicyV3 :: P.BuiltinData -> P.BuiltinUnit
mkMintTokenNamePolicyV3 arg = if checkTokenName then BI.unitval else P.traceError "wrong token name"
 where
  -- ctx@(V3.ScriptContext info (V3.Redeemer redeemer) _scriptInfo) =
  --     V3.unsafeFromBuiltinData arg

  context :: BI.BuiltinList P.BuiltinData
  context = constrArgs arg

  txInfoBuiltinData :: P.BuiltinData
  txInfoBuiltinData = BI.head context

  txInfo :: V3.TxInfo
  txInfo = V3.unsafeFromBuiltinData txInfoBuiltinData

  redeemerFollowedByScriptInfo :: BI.BuiltinList P.BuiltinData
  redeemerFollowedByScriptInfo = BI.tail context

  redeemerBuiltinData :: P.BuiltinData
  redeemerBuiltinData = BI.head redeemerFollowedByScriptInfo

  redeemer :: V1.TokenName
  redeemer = V3.unsafeFromBuiltinData redeemerBuiltinData

  scriptInfoData :: P.BuiltinData
  scriptInfoData = BI.head (BI.tail redeemerFollowedByScriptInfo)

  scriptInfo :: V3.ScriptInfo
  scriptInfo = V3.unsafeFromBuiltinData scriptInfoData

  -- TODO: Use builtin when available in PV3
  ownCurrencySymbol :: V3.ScriptInfo -> V3.CurrencySymbol
  ownCurrencySymbol = \case
    V3.MintingScript cs -> cs
    _ -> P.traceError "Lh"

  checkTokenName :: Bool
  checkTokenName =
    V1.valueOf (V3.mintValueMinted (V3.txInfoMint txInfo)) (ownCurrencySymbol scriptInfo) redeemer
      P.> 0

-- Time range policy --

{-# INLINEABLE mkTimeRangePolicyV3 #-}
mkTimeRangePolicyV3 :: P.BuiltinData -> P.BuiltinUnit
mkTimeRangePolicyV3 arg = if checkRange then BI.unitval else P.traceError "not in valid range"
 where
  context :: BI.BuiltinList P.BuiltinData
  context = constrArgs arg

  txInfoBuiltinData :: P.BuiltinData
  txInfoBuiltinData = BI.head context

  txInfo :: V3.TxInfo
  txInfo = V3.unsafeFromBuiltinData txInfoBuiltinData

  redeemerFollowedByScriptInfo :: BI.BuiltinList P.BuiltinData
  redeemerFollowedByScriptInfo = BI.tail context

  redeemerBuiltinData :: P.BuiltinData
  redeemerBuiltinData = BI.head redeemerFollowedByScriptInfo

  redeemer :: V1.POSIXTime
  redeemer = V3.unsafeFromBuiltinData redeemerBuiltinData

  range :: V1.POSIXTimeRange
  range = V3.txInfoValidRange txInfo

  checkRange :: Bool
  checkRange = V1.to redeemer `V1.contains` range

-- Witness redeemer policy --

{-# INLINEABLE mkWitnessRedeemerPolicyV3 #-}
mkWitnessRedeemerPolicyV3 :: P.BuiltinData -> P.BuiltinUnit
mkWitnessRedeemerPolicyV3 arg =
  if checkWitness then BI.unitval else P.traceError "not signed by redeemer pubkeyhash"
 where
  context :: BI.BuiltinList P.BuiltinData
  context = constrArgs arg

  txInfoBuiltinData :: P.BuiltinData
  txInfoBuiltinData = BI.head context

  txInfo :: V3.TxInfo
  txInfo = V3.unsafeFromBuiltinData txInfoBuiltinData

  redeemerFollowedByScriptInfo :: BI.BuiltinList P.BuiltinData
  redeemerFollowedByScriptInfo = BI.tail context

  redeemerBuiltinData :: P.BuiltinData
  redeemerBuiltinData = BI.head redeemerFollowedByScriptInfo

  redeemer :: V3.PubKeyHash
  redeemer = V3.unsafeFromBuiltinData redeemerBuiltinData

  -- TODO: Use builtin when available in PV3
  txSignedBy :: V3.TxInfo -> V3.PubKeyHash -> Bool
  txSignedBy V3.TxInfo{V3.txInfoSignatories} k =
    case List.find (k P.==) txInfoSignatories of
      P.Just _ -> P.True
      P.Nothing -> P.False

  checkWitness :: Bool
  checkWitness = txSignedBy txInfo redeemer

-- PlutusV4 policies --

-- V4 contexts and transaction information are list-encoded, unlike V3.
{-# INLINEABLE mkMintTokenNamePolicyV4 #-}
mkMintTokenNamePolicyV4 :: P.BuiltinData -> P.BuiltinUnit
mkMintTokenNamePolicyV4 arg =
  if checkTokenName then BI.unitval else P.traceError "wrong token name"
 where
  context :: V4.ScriptContext
  context = V4.unsafeFromBuiltinData arg

  V4.Redeemer redeemerData = V4.scriptContextRedeemer context

  redeemer :: V4.TokenName
  redeemer = V4.unsafeFromBuiltinData redeemerData

  checkTokenName :: Bool
  checkTokenName =
    V4.valueOf
      (V4.mintValueMinted (V4.txInfoMint (V4.scriptContextTxInfo context)))
      (V4Contexts.ownCurrencySymbol context)
      redeemer
      P.> 0

{-# INLINEABLE mkTimeRangePolicyV4 #-}
mkTimeRangePolicyV4 :: P.BuiltinData -> P.BuiltinUnit
mkTimeRangePolicyV4 arg =
  if checkRange then BI.unitval else P.traceError "not in valid range"
 where
  context :: V4.ScriptContext
  context = V4.unsafeFromBuiltinData arg

  V4.Redeemer redeemerData = V4.scriptContextRedeemer context

  redeemer :: V4.POSIXTime
  redeemer = V4.unsafeFromBuiltinData redeemerData

  -- V4 validity ranges have an inclusive lower and an exclusive upper bound.
  checkRange :: Bool
  checkRange = case V4.untilExclusive (V4.txInfoValidRange (V4.scriptContextTxInfo context)) of
    P.Just upper -> upper P.<= redeemer
    P.Nothing -> P.False

{-# INLINEABLE mkWitnessRedeemerPolicyV4 #-}
mkWitnessRedeemerPolicyV4 :: P.BuiltinData -> P.BuiltinUnit
mkWitnessRedeemerPolicyV4 arg =
  if checkWitness then BI.unitval else P.traceError "not signed by redeemer pubkeyhash"
 where
  context :: V4.ScriptContext
  context = V4.unsafeFromBuiltinData arg

  V4.Redeemer redeemerData = V4.scriptContextRedeemer context

  redeemer :: V4.PubKeyHash
  redeemer = V4.unsafeFromBuiltinData redeemerData

  -- V4 replaces txInfoSignatories with credentials in txInfoGuards.
  checkWitness :: Bool
  checkWitness = V4Contexts.txSignedBy (V4.scriptContextTxInfo context) redeemer
