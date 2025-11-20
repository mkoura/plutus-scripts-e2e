module Main (main) where

import Control.Monad (zipWithM_)
import Data.Text qualified as T
import Helpers.ScriptUtils (ScriptGroup (ScriptGroup, sgBaseName, sgScripts))
import Main.Utf8 (withUtf8)
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV2, PlutusV3))
import PlutusLedgerApi.Envelope qualified as Envelope
import PlutusScripts.Basic.V_1_1 qualified as Basic
import PlutusScripts.Batch6.V3_110 qualified as Batch6_V3_110
import PlutusScripts.Bitwise.V_1_0 qualified as BitwiseV0
import PlutusScripts.Bitwise.V_1_1 qualified as BitwiseV1
import PlutusScripts.Hashing.V_1_1 qualified as Hashing
import PlutusScripts.SECP256k1.V_1_1 qualified as SECP
import PlutusTx.Code (CompiledCode)
import System.Directory (createDirectoryIfMissing)

--------------------------------------------------------------------------------
-- Script Group Helpers --------------------------------------------------------

-- | Write a group of numbered scripts (e.g., script_1.plutus, script_2.plutus, ...)
writeScriptGroup :: ScriptGroup DefaultUni DefaultFun a -> IO ()
writeScriptGroup ScriptGroup{..} =
  zipWithM_ writeNumbered [1 :: Integer ..] sgScripts
 where
  writeNumbered n code = writeEnvelopeV3 (sgBaseName ++ "_" ++ show n) code

--------------------------------------------------------------------------------
-- Main ------------------------------------------------------------------------

main :: IO ()
main = withUtf8 do
  -- Bitwise V1.0 script (PlutusV2)
  writeEnvelopeV2
    "byteStringToIntegerRoundtripPolicyV2"
    BitwiseV0.byteStringToIntegerRoundtripPolicyCompiledV2

  -- Basic scripts (PlutusV3)
  writeEnvelopeV3 "alwaysSucceedPolicyScriptV3" Basic.alwaysSucceedPolicyCompiled
  writeEnvelopeV3 "alwaysFailsPolicyScriptV3" Basic.alwaysFailsPolicyCompiled
  writeEnvelopeV3 "mintTokenNamePolicyScriptV3" Basic.mintTokenNamePolicyCompiledV3
  writeEnvelopeV3 "timeRangePolicyScriptV3" Basic.timeRangePolicyCompiledV3
  writeEnvelopeV3 "witnessRedeemerPolicyScriptV3" Basic.witnessRedeemerPolicyCompiledV3

  -- SECP256k1 scripts (PlutusV3)
  writeEnvelopeV3 "verifySchnorrPolicyScriptV3" SECP.verifySchnorrPolicyCompiledV3
  writeEnvelopeV3 "verifyEcdsaPolicyScriptV3" SECP.verifyEcdsaPolicyCompiledV3

  -- Hashing scripts (PlutusV3)
  writeEnvelopeV3 "succeedingRipemd_160Policy" Hashing.succeedingRipemd_160PolicyCompiled

  -- Bitwise V1.1 scripts (PlutusV3)
  writeEnvelopeV3
    "succeedingAndByteStringPolicyScriptV3"
    BitwiseV1.succeedingAndByteStringPolicyCompiledV3
  writeEnvelopeV3
    "succeedingOrByteStringPolicyScriptV3"
    BitwiseV1.succeedingOrByteStringPolicyCompiledV3
  writeEnvelopeV3
    "succeedingXorByteStringPolicyScriptV3"
    BitwiseV1.succeedingXorByteStringPolicyCompiledV3
  writeEnvelopeV3
    "succeedingComplementByteStringPolicyScriptV3"
    BitwiseV1.succeedingComplementByteStringPolicyCompiledV3
  writeEnvelopeV3
    "succeedingShiftByteStringPolicyScriptV3"
    BitwiseV1.succeedingShiftByteStringPolicyCompiledV3
  writeEnvelopeV3
    "succeedingRotateByteStringPolicyScriptV3"
    BitwiseV1.succeedingRotateByteStringPolicyCompiledV3
  writeEnvelopeV3
    "succeedingCountSetBitsPolicyScriptV3"
    BitwiseV1.succeedingCountSetBitsPolicyCompiledV3
  writeEnvelopeV3
    "succeedingFindFirstSetBitPolicyScriptV3"
    BitwiseV1.succeedingFindFirstSetBitPolicyCompiledV3
  writeEnvelopeV3
    "succeedingReadBitPolicyScriptV3"
    BitwiseV1.succeedingReadBitPolicyCompiledV3
  writeEnvelopeV3
    "succeedingWriteBitsPolicyScriptV3"
    BitwiseV1.succeedingWriteBitsPolicyCompiledV3
  writeEnvelopeV3
    "succeedingReplicateBytePolicyScriptV3"
    BitwiseV1.succeedingReplicateBytePolicyCompiledV3

  -- Failing Bitwise Tests (ReadBit, WriteBits, ReplicateByte variants)
  mapM_ writeScriptGroup BitwiseV1.failingBitwiseScriptGroupsV3

  -- \** Batch 6 (protocol version 11) **
  writeEnvelopeV3
    "succeedingDropListPolicyScript_V3_110"
    Batch6_V3_110.succeedingDropListPolicyCompiled_V3_110

  writeScriptGroup Batch6_V3_110.expensiveDropListScriptGroup_V3_110

--------------------------------------------------------------------------------
-- IO helpers ------------------------------------------------------------------

-- | Write compiled Plutus code to a .plutus file
writeEnvelope :: PlutusLedgerLanguage -> FilePath -> CompiledCode a -> IO ()
writeEnvelope lang filename compiledCode = do
  let dir = "serialised-plutus-scripts"
  let filePath = dir ++ "/" ++ filename ++ ".plutus"
  let description = T.pack filename
  createDirectoryIfMissing True dir
  Envelope.writeCodeEnvelopeForVersion lang description compiledCode filePath

-- | Write PlutusV2 script
writeEnvelopeV2 :: FilePath -> CompiledCode a -> IO ()
writeEnvelopeV2 = writeEnvelope PlutusV2

-- | Write PlutusV3 script
writeEnvelopeV3 :: FilePath -> CompiledCode a -> IO ()
writeEnvelopeV3 = writeEnvelope PlutusV3
