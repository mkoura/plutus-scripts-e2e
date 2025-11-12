module Main (main) where

import Data.Text qualified as T
import Main.Utf8 (withUtf8)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV2, PlutusV3))
import PlutusLedgerApi.Envelope qualified as Envelope
import PlutusScripts.Basic.V_1_1 qualified as Basic
import PlutusScripts.Bitwise.V_1_0 qualified as BitwiseV0
import PlutusScripts.Bitwise.V_1_1 qualified as BitwiseV1
import PlutusScripts.Hashing.V_1_1 qualified as Hashing
import PlutusScripts.SECP256k1.V_1_1 qualified as SECP
import PlutusTx.Code (CompiledCode)
import System.Directory (createDirectoryIfMissing)

-- | Write compiled Plutus code to a .plutus file
writeCompiledScript ::
    PlutusLedgerLanguage -> FilePath -> CompiledCode a -> IO ()
writeCompiledScript lang filename compiledCode = do
    let dir = "serialised-plutus-scripts"
        filePath = dir ++ "/" ++ filename ++ ".plutus"
    createDirectoryIfMissing True dir
    Envelope.writeCodeEnvelopeForVersion lang (T.pack "") compiledCode filePath

main :: IO ()
main = withUtf8 do
    -- Bitwise V1.0 script (PlutusV2)
    writeCompiledScript PlutusV2 "byteStringToIntegerRoundtripPolicyV2" BitwiseV0.byteStringToIntegerRoundtripPolicyCompiledV2

    -- Basic scripts (PlutusV3)
    writeCompiledScript PlutusV3 "alwaysSucceedPolicyScriptV3" Basic.alwaysSucceedPolicyCompiled
    writeCompiledScript PlutusV3 "alwaysFailsPolicyScriptV3" Basic.alwaysFailsPolicyCompiled
    writeCompiledScript PlutusV3 "mintTokenNamePolicyScriptV3" Basic.mintTokenNamePolicyCompiledV3
    writeCompiledScript PlutusV3 "timeRangePolicyScriptV3" Basic.timeRangePolicyCompiledV3
    writeCompiledScript PlutusV3 "witnessRedeemerPolicyScriptV3" Basic.witnessRedeemerPolicyCompiledV3

    -- SECP256k1 scripts (PlutusV3)
    writeCompiledScript PlutusV3 "verifySchnorrPolicyScriptV3" SECP.verifySchnorrPolicyCompiledV3
    writeCompiledScript PlutusV3 "verifyEcdsaPolicyScriptV3" SECP.verifyEcdsaPolicyCompiledV3

    -- Hashing scripts (PlutusV3)
    writeCompiledScript PlutusV3 "succeedingRipemd_160Policy" Hashing.succeedingRipemd_160PolicyCompiled

    -- Bitwise V1.1 scripts (PlutusV3)
    writeCompiledScript PlutusV3 "succeedingAndByteStringPolicyScriptV3" BitwiseV1.succeedingAndByteStringPolicyCompiledV3
    writeCompiledScript PlutusV3 "succeedingOrByteStringPolicyScriptV3" BitwiseV1.succeedingOrByteStringPolicyCompiledV3
    writeCompiledScript PlutusV3 "succeedingXorByteStringPolicyScriptV3" BitwiseV1.succeedingXorByteStringPolicyCompiledV3
    writeCompiledScript PlutusV3 "succeedingComplementByteStringPolicyScriptV3" BitwiseV1.succeedingComplementByteStringPolicyCompiledV3
    writeCompiledScript PlutusV3 "succeedingShiftByteStringPolicyScriptV3" BitwiseV1.succeedingShiftByteStringPolicyCompiledV3
    writeCompiledScript PlutusV3 "succeedingRotateByteStringPolicyScriptV3" BitwiseV1.succeedingRotateByteStringPolicyCompiledV3
    writeCompiledScript PlutusV3 "succeedingCountSetBitsPolicyScriptV3" BitwiseV1.succeedingCountSetBitsPolicyCompiledV3
    writeCompiledScript PlutusV3 "succeedingFindFirstSetBitPolicyScriptV3" BitwiseV1.succeedingFindFirstSetBitPolicyCompiledV3
    writeCompiledScript PlutusV3 "succeedingReadBitPolicyScriptV3" BitwiseV1.succeedingReadBitPolicyCompiledV3
    writeCompiledScript PlutusV3 "succeedingWriteBitsPolicyScriptV3" BitwiseV1.succeedingWriteBitsPolicyCompiledV3
    writeCompiledScript PlutusV3 "succeedingReplicateBytePolicyScriptV3" BitwiseV1.succeedingReplicateBytePolicyCompiledV3
