{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
-- Not using all CardanoEra
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module PlutusScripts.Governance.Common where

import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.AssocMap qualified as AM
import PlutusTx.List qualified as List
import PlutusTx.Prelude qualified as P

-- TODO: compare cost of this implementation with PlutusTx's length
{-# INLINEABLE _lengthEq #-}
_lengthEq :: [a] -> [b] -> Bool
_lengthEq [] [] = True
_lengthEq (_ : xs) (_ : ys) = _lengthEq xs ys
_lengthEq _ _ = False

{-# INLINEABLE listEq #-}
listEq :: (P.Eq a) => [a] -> [a] -> Bool
listEq rs cs =
  List.length rs
    P.== List.length cs
    P.&& List.all (P.== True) (List.zipWith (P.==) rs cs)

-- ScriptInfo --

{-# INLINEABLE mkVerifyScriptInfo #-}
mkVerifyScriptInfo :: V3.ScriptInfo -> V3.ScriptContext -> Bool
mkVerifyScriptInfo _r _sc = False

-- TxCert --

{-# INLINEABLE mkVerifyTxCerts #-}
mkVerifyTxCerts :: [V3.TxCert] -> V3.ScriptContext -> Bool
mkVerifyTxCerts r sc = r P.== V3.txInfoTxCerts (V3.scriptContextTxInfo sc)

-- txInfoVotes --

{-# INLINEABLE mkVerifyVotes #-}
mkVerifyVotes :: V3.Map V3.Voter (V3.Map V3.GovernanceActionId V3.Vote) -> V3.ScriptContext -> Bool
mkVerifyVotes r sc = do
  let redeemerVoters = AM.keys r
      contextVoters = AM.keys $ V3.txInfoVotes P.$ V3.scriptContextTxInfo sc
      redeemerGovActionIds = AM.keys <$> AM.elems r
      contextGovActionIds = AM.keys <$> AM.elems r
      redeemerVotes = AM.elems <$> AM.elems r
      contextVotes = AM.elems <$> AM.elems r
  listEq redeemerVoters contextVoters
    P.&& emListEq redeemerGovActionIds contextGovActionIds
    P.&& emListEq redeemerVotes contextVotes
 where
  {-# INLINEABLE emListEq #-}
  emListEq :: (P.Eq a) => [[a]] -> [[a]] -> Bool
  emListEq rs cs =
    List.length rs
      P.== List.length cs
      -- lengthEq rs cs -- alternate implementation
      P.&& List.all (P.== True) (List.zipWith listEq rs cs)

-- txInfoProposalProcedures --

{-# INLINEABLE mkVerifyProposalProcedures #-}
mkVerifyProposalProcedures :: [V3.ProposalProcedure] -> V3.ScriptContext -> Bool
mkVerifyProposalProcedures _r _sc =
  -- r P.== V3.txInfoProposalProcedures (V3.scriptContextTxInfo sc)
  False

-- txInfoCurrentTreasuryAmount --

{-# INLINEABLE mkVerifyCurrentTreasuryAmount #-}
mkVerifyCurrentTreasuryAmount :: P.Maybe V3.Lovelace -> V3.ScriptContext -> Bool
mkVerifyCurrentTreasuryAmount r sc =
  r P.== V3.txInfoCurrentTreasuryAmount (V3.scriptContextTxInfo sc)

-- txInfoTreasuryDonation --

{-# INLINEABLE mkVerifyTreasuryDonation #-}
mkVerifyTreasuryDonation :: P.Maybe V3.Lovelace -> V3.ScriptContext -> Bool
mkVerifyTreasuryDonation r sc =
  r P.== V3.txInfoTreasuryDonation (V3.scriptContextTxInfo sc)

