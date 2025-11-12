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
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.Governance.Common (
  mkVerifyCurrentTreasuryAmount,
  mkVerifyProposalProcedures,
  mkVerifyScriptInfo,
  mkVerifyTreasuryDonation,
  mkVerifyTxCerts,
  mkVerifyVotes,
 )
import PlutusTx (compile)

-- ScriptInfo --

verifyScriptInfoPolicy :: V3.SerialisedScript
verifyScriptInfoPolicy =
  V3.serialiseCompiledCode
    $$(compile [||mkUntypedMintingPolicy mkVerifyScriptInfo||])

-- TxCert --

verifyTxCertsPolicy :: V3.SerialisedScript
verifyTxCertsPolicy = V3.serialiseCompiledCode $$(compile [||wrap||])
 where
  wrap = mkUntypedMintingPolicy mkVerifyTxCerts

-- txInfoVotingProcedures --

verifyVotesPolicy :: V3.SerialisedScript
verifyVotesPolicy = V3.serialiseCompiledCode $$(compile [||wrap||])
 where
  wrap = mkUntypedMintingPolicy mkVerifyVotes

-- txInfoProposalProcedures --

verifyProposalProceduresPolicy :: V3.SerialisedScript
verifyProposalProceduresPolicy = V3.serialiseCompiledCode $$(compile [||wrap||])
 where
  wrap = mkUntypedMintingPolicy mkVerifyProposalProcedures

-- txInfoCurrentTreasuryAmount --

verifyCurrentTreasuryAmountPolicy :: V3.SerialisedScript
verifyCurrentTreasuryAmountPolicy = V3.serialiseCompiledCode $$(compile [||wrap||])
 where
  wrap = mkUntypedMintingPolicy mkVerifyCurrentTreasuryAmount

-- txInfoTreasuryDonation --

verifyTreasuryDonationPolicy :: V3.SerialisedScript
verifyTreasuryDonationPolicy = V3.serialiseCompiledCode $$(compile [||wrap||])
 where
  wrap = mkUntypedMintingPolicy mkVerifyTreasuryDonation
