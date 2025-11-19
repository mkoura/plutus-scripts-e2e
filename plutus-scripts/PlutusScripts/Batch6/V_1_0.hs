{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module PlutusScripts.Batch6.V_1_0 where

import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion100)
-- import PlutusLedgerApi.V3 qualified as V3
import PlutusScripts.Batch6.DropList (mkDropListPolicyI, succeedingDropListParamsI)
import PlutusTx (compile, liftCode, unsafeApplyCode)
import PlutusTx.Code (CompiledCodeIn)
import PlutusTx.Prelude qualified as P
{-
succeedingDropListPolicy :: V3.SerialisedScript
succeedingDropListPolicy =
  V3.serialiseCompiledCode $
    $$(compile [|| wrap ||])
  where wrap = mkUntypedMintingPolicy @V3.ScriptContext mkDropListPolicyI
-}


-- Compiled code values with parameters already applied for succeeding tests
succeedingDropListPolicyCompiledV2
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingDropListPolicyCompiledV2 = 
  $$(compile [|| mkDropListPolicyI ||])
    `unsafeApplyCode` liftCode plcVersion100 succeedingDropListParamsI
