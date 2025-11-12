{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.BLS.AggregateSigWithMultipleKeys.V_1_1 where

import Helpers.ScriptUtils (IsScriptContext (mkUntypedMintingPolicy))
import PlutusCore.Version (plcVersion110)
import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.BLS.AggregateSigWithMultipleKeys.Common (aggregateMultiKeyG2Script)
import PlutusScripts.BLS.Common (blsSigBls12381G2XmdSha256SswuRoNul, byteString16Null)
import PlutusTx (compile, liftCode, unsafeApplyCode)

verifyBlsAggregateSigMultiKeyG2PolicyV3 :: V3.SerialisedScript
verifyBlsAggregateSigMultiKeyG2PolicyV3 =
  V3.serialiseCompiledCode $
    $$( compile
          [||\a b -> mkUntypedMintingPolicy @V3.ScriptContext (aggregateMultiKeyG2Script a b)||]
      )
      `unsafeApplyCode` liftCode plcVersion110 byteString16Null
      `unsafeApplyCode` liftCode plcVersion110 blsSigBls12381G2XmdSha256SswuRoNul
