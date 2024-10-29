module PlutusScripts.Hashing.Ripemd_160 where

writeSuceedingV3Script
  :: PlutusTx.Lift DefaultUni [param]
  => String
  -> (PlutusTx.CompiledCodeIn DefaultUni DefaultFun ([param] -> r))
  -> [param]
  -> IO ()
writeSuceedingV3Script name code params =
  let script :: C.PlutusScript C.PlutusScriptV3
      script = C.PlutusScriptSerialised $ serialiseCompiledCode (code `PlutusTx.unsafeApplyCode` (PlutusTx.liftCode plcVersion110 params))
  in writeSerialisedScript name script


