{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusScripts.Basic.Common where

import Helpers.ScriptUtils (check, constrArgs)
import PlutusLedgerApi.V1 qualified as P
import PlutusLedgerApi.V1.Interval qualified as P
import PlutusLedgerApi.V1.Value qualified as P
import PlutusLedgerApi.V3 qualified as PV3
import PlutusTx.Builtins.Internal qualified as BI (
  BuiltinList,
  head,
  tail,
  unitval,
 )
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
  -- ctx@(PV3.ScriptContext info (PV3.Redeemer redeemer) _scriptInfo) =
  --     PV3.unsafeFromBuiltinData arg

  context :: BI.BuiltinList P.BuiltinData
  context = constrArgs arg

  txInfoBuiltinData :: P.BuiltinData
  txInfoBuiltinData = BI.head context

  txInfo :: PV3.TxInfo
  txInfo = PV3.unsafeFromBuiltinData txInfoBuiltinData

  redeemerFollowedByScriptInfo :: BI.BuiltinList P.BuiltinData
  redeemerFollowedByScriptInfo = BI.tail context

  redeemerBuiltinData :: P.BuiltinData
  redeemerBuiltinData = BI.head redeemerFollowedByScriptInfo

  redeemer :: P.TokenName
  redeemer = PV3.unsafeFromBuiltinData redeemerBuiltinData

  scriptInfoData :: P.BuiltinData
  scriptInfoData = BI.head (BI.tail redeemerFollowedByScriptInfo)

  scriptInfo :: PV3.ScriptInfo
  scriptInfo = PV3.unsafeFromBuiltinData scriptInfoData

  -- TODO: Use builtin when available in PV3
  ownCurrencySymbol :: PV3.ScriptInfo -> PV3.CurrencySymbol
  ownCurrencySymbol = \case
    PV3.MintingScript cs -> cs
    _ -> P.traceError "Lh"

  checkTokenName :: Bool
  checkTokenName = P.valueOf (PV3.txInfoMint txInfo) (ownCurrencySymbol scriptInfo) redeemer P.> 0

-- Time range policy --

{-# INLINEABLE mkTimeRangePolicyV3 #-}
mkTimeRangePolicyV3 :: P.BuiltinData -> P.BuiltinUnit
mkTimeRangePolicyV3 arg = if checkRange then BI.unitval else P.traceError "not in valid range"
 where
  context :: BI.BuiltinList P.BuiltinData
  context = constrArgs arg

  txInfoBuiltinData :: P.BuiltinData
  txInfoBuiltinData = BI.head context

  txInfo :: PV3.TxInfo
  txInfo = PV3.unsafeFromBuiltinData txInfoBuiltinData

  redeemerFollowedByScriptInfo :: BI.BuiltinList P.BuiltinData
  redeemerFollowedByScriptInfo = BI.tail context

  redeemerBuiltinData :: P.BuiltinData
  redeemerBuiltinData = BI.head redeemerFollowedByScriptInfo

  redeemer :: P.POSIXTime
  redeemer = PV3.unsafeFromBuiltinData redeemerBuiltinData

  range :: P.POSIXTimeRange
  range = PV3.txInfoValidRange txInfo

  checkRange :: Bool
  checkRange = P.to redeemer `P.contains` range

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

  txInfo :: PV3.TxInfo
  txInfo = PV3.unsafeFromBuiltinData txInfoBuiltinData

  redeemerFollowedByScriptInfo :: BI.BuiltinList P.BuiltinData
  redeemerFollowedByScriptInfo = BI.tail context

  redeemerBuiltinData :: P.BuiltinData
  redeemerBuiltinData = BI.head redeemerFollowedByScriptInfo

  redeemer :: PV3.PubKeyHash
  redeemer = PV3.unsafeFromBuiltinData redeemerBuiltinData

  -- TODO: Use builtin when available in PV3
  txSignedBy :: PV3.TxInfo -> PV3.PubKeyHash -> Bool
  txSignedBy PV3.TxInfo{PV3.txInfoSignatories} k =
    case P.find (k P.==) txInfoSignatories of
      P.Just _ -> P.True
      P.Nothing -> P.False

  checkWitness :: Bool
  checkWitness = txSignedBy txInfo redeemer
