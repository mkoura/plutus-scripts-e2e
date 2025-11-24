module Main (main) where

import Data.Text qualified as T
import Main.Utf8 (withUtf8)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV1, PlutusV2, PlutusV3))
import PlutusLedgerApi.Envelope qualified as Envelope
import PlutusScripts.Batch6.Array.V1_100 qualified as Array_V1_100
import PlutusScripts.Batch6.Array.V1_110 qualified as Array_V1_110
import PlutusScripts.Batch6.Array.V2_100 qualified as Array_V2_100
import PlutusScripts.Batch6.Array.V2_110 qualified as Array_V2_110
import PlutusScripts.Batch6.Array.V3_100 qualified as Array_V3_100
import PlutusScripts.Batch6.Array.V3_110 qualified as Array_V3_110
import PlutusScripts.Basic.V_1_1 qualified as Basic
import Control.Monad (zipWithM_)
import Helpers.ScriptUtils (ScriptGroup (ScriptGroup, sgBaseName, sgScripts))
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusScripts.Bitwise.V_1_0 qualified as BitwiseV0
import PlutusScripts.Bitwise.V_1_1 qualified as BitwiseV1
import PlutusScripts.Hashing.V_1_1 qualified as Hashing
import PlutusScripts.SECP256k1.V_1_1 qualified as SECP
import PlutusTx.Code (CompiledCode)
import System.Directory (createDirectoryIfMissing)


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

  -- Array builtin scripts (V1/1.0.0)
  writeEnvelopeV1 "succeedingIndexArrayPolicyScript_V1_100"
    Array_V1_100.succeedingIndexArrayPolicy
  writeEnvelopeV1 "succeedingLengthOfArrayPolicyScript_V1_100"
    Array_V1_100.succeedingLengthOfArrayPolicy
  writeEnvelopeV1 "succeedingListToArrayPolicyScript_V1_100"
    Array_V1_100.succeedingListToArrayPolicy

  -- Array builtin scripts (V1/1.1.0)
  writeEnvelopeV1 "succeedingIndexArrayPolicyScript_V1_110"
    Array_V1_110.succeedingIndexArrayPolicy
  writeEnvelopeV1 "succeedingLengthOfArrayPolicyScript_V1_110"
    Array_V1_110.succeedingLengthOfArrayPolicy
  writeEnvelopeV1 "succeedingListToArrayPolicyScript_V1_110"
    Array_V1_110.succeedingListToArrayPolicy

  -- Array builtin scripts (V2/1.0.0)
  writeEnvelopeV2 "succeedingIndexArrayPolicyScript_V2_100"
    Array_V2_100.succeedingIndexArrayPolicy
  writeEnvelopeV2 "succeedingLengthOfArrayPolicyScript_V2_100"
    Array_V2_100.succeedingLengthOfArrayPolicy
  writeEnvelopeV2 "succeedingListToArrayPolicyScript_V2_100"
    Array_V2_100.succeedingListToArrayPolicy

  -- Array builtin scripts (V2/1.1.0)
  writeEnvelopeV2 "succeedingIndexArrayPolicyScript_V2_110"
    Array_V2_110.succeedingIndexArrayPolicy
  writeEnvelopeV2 "succeedingLengthOfArrayPolicyScript_V2_110"
    Array_V2_110.succeedingLengthOfArrayPolicy
  writeEnvelopeV2 "succeedingListToArrayPolicyScript_V2_110"
    Array_V2_110.succeedingListToArrayPolicy

  -- Array builtin scripts (V3/1.0.0)
  writeEnvelopeV3 "succeedingIndexArrayPolicyScript_V3_100"
    Array_V3_100.succeedingIndexArrayPolicy
  writeEnvelopeV3 "succeedingLengthOfArrayPolicyScript_V3_100"
    Array_V3_100.succeedingLengthOfArrayPolicy
  writeEnvelopeV3 "succeedingListToArrayPolicyScript_V3_100"
    Array_V3_100.succeedingListToArrayPolicy

  -- Array builtin scripts (V3/1.1.0)
  writeEnvelopeV3 "succeedingIndexArrayPolicyScript_V3_110"
    Array_V3_110.succeedingIndexArrayPolicy
  writeEnvelopeV3 "succeedingLengthOfArrayPolicyScript_V3_110"
    Array_V3_110.succeedingLengthOfArrayPolicy
  writeEnvelopeV3 "succeedingListToArrayPolicyScript_V3_110"
    Array_V3_110.succeedingListToArrayPolicy

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

-- | Write PlutusV1 script
writeEnvelopeV1 :: FilePath -> CompiledCode a -> IO ()
writeEnvelopeV1 = writeEnvelope PlutusV1

-- | Write PlutusV2 script
writeEnvelopeV2 :: FilePath -> CompiledCode a -> IO ()
writeEnvelopeV2 = writeEnvelope PlutusV2

-- | Write PlutusV3 script
writeEnvelopeV3 :: FilePath -> CompiledCode a -> IO ()
writeEnvelopeV3 = writeEnvelope PlutusV3

-- | Write a group of numbered scripts (e.g., script_1.plutus, script_2.plutus, ...)
writeScriptGroup :: ScriptGroup DefaultUni DefaultFun a -> IO ()
writeScriptGroup ScriptGroup{..} =
  zipWithM_ writeNumbered [1 :: Integer ..] sgScripts
 where
  writeNumbered n = writeEnvelopeV3 (sgBaseName ++ "_" ++ show n)
