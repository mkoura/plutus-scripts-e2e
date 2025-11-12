{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Governance.V_1_1 where

import Helpers.ScriptUtils (mkUntypedMintingPolicy)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusScripts.Governance.Common (
    mkVerifyCurrentTreasuryAmount,
    mkVerifyProposalProcedures,
    mkVerifyScriptInfo,
    mkVerifyTreasuryDonation,
    mkVerifyTxCerts,
    mkVerifyVotes,
 )
import PlutusTx qualified

-- ScriptInfo --

verifyScriptInfoPolicy :: SerialisedScript
verifyScriptInfoPolicy =
    serialiseCompiledCode
        $$(PlutusTx.compile [||mkUntypedMintingPolicy mkVerifyScriptInfo||])

-- TxCert --

verifyTxCertsPolicy :: SerialisedScript
verifyTxCertsPolicy = serialiseCompiledCode $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy mkVerifyTxCerts

-- txInfoVotingProcedures --

verifyVotesPolicy :: SerialisedScript
verifyVotesPolicy = serialiseCompiledCode $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy mkVerifyVotes

-- txInfoProposalProcedures --

verifyProposalProceduresPolicy :: SerialisedScript
verifyProposalProceduresPolicy = serialiseCompiledCode $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy mkVerifyProposalProcedures

-- txInfoCurrentTreasuryAmount --

verifyCurrentTreasuryAmountPolicy :: SerialisedScript
verifyCurrentTreasuryAmountPolicy = serialiseCompiledCode $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy mkVerifyCurrentTreasuryAmount

-- txInfoTreasuryDonation --

verifyTreasuryDonationPolicy :: SerialisedScript
verifyTreasuryDonationPolicy = serialiseCompiledCode $$(PlutusTx.compile [||wrap||])
  where
    wrap = mkUntypedMintingPolicy mkVerifyTreasuryDonation
