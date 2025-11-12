{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.Groth16.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.BLS.Groth16.Common (
  CompressedG1Element (compressedG1),
  CompressedG2Element (compressedG2),
  groth16a,
  groth16alpha,
  groth16b,
  groth16beta,
  groth16c,
  groth16delta,
  groth16gamma,
  groth16gamma_abc_1,
  groth16gamma_abc_2,
  verifyBlsGroth16Script,
 )
import PlutusTx (compile, liftCode, unsafeApplyCode)

{- | Make a UPLC script applying groth16Verify to the inputs.  Passing the
 newtype inputs increases the size and CPU cost slightly, so we unwrap them
 first.  This should return `True`.
-}
verifyBlsGroth16PolicyV3 :: V3.SerialisedScript
verifyBlsGroth16PolicyV3 =
  V3.serialiseCompiledCode $
    $$( compile
          [||
          \a b c d e f g h i ->
            mkUntypedMintingPolicy @V3.ScriptContext
              (verifyBlsGroth16Script a b c d e f g h i)
          ||]
      )
      `unsafeApplyCode` liftCode plcVersion110 (compressedG1 groth16alpha)
      `unsafeApplyCode` liftCode plcVersion110 (compressedG2 groth16beta)
      `unsafeApplyCode` liftCode plcVersion110 (compressedG2 groth16gamma)
      `unsafeApplyCode` liftCode plcVersion110 (compressedG2 groth16delta)
      `unsafeApplyCode` liftCode plcVersion110 (compressedG1 groth16gamma_abc_1)
      `unsafeApplyCode` liftCode plcVersion110 (compressedG1 groth16gamma_abc_2)
      `unsafeApplyCode` liftCode plcVersion110 (compressedG1 groth16a)
      `unsafeApplyCode` liftCode plcVersion110 (compressedG2 groth16b)
      `unsafeApplyCode` liftCode plcVersion110 (compressedG1 groth16c)
