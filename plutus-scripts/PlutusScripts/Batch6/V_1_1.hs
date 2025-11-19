{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module PlutusScripts.Batch6.V_1_1 where

import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusCore.Version (plcVersion110)
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
succeedingDropListPolicyCompiledV3
  :: CompiledCodeIn DefaultUni DefaultFun (P.BuiltinData -> P.BuiltinUnit)
succeedingDropListPolicyCompiledV3 = 
  $$(compile [|| mkDropListPolicyI ||])
    `unsafeApplyCode` liftCode plcVersion110 succeedingDropListParamsI
