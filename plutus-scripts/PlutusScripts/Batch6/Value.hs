module PlutusScripts.Batch6.Value (v3ValueScripts) where

import Helpers.Envelopes (VersionedScript (VersionedScript), plc100, plc110)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV1, PlutusV2, PlutusV3))
-- import PlutusScripts.Batch6.Value.V1_100 qualified as V1_100
-- import PlutusScripts.Batch6.Value.V1_110 qualified as V1_110
-- import PlutusScripts.Batch6.Value.V2_100 qualified as V2_100
-- import PlutusScripts.Batch6.Value.V2_110 qualified as V2_110
-- import PlutusScripts.Batch6.Value.V3_100 qualified as V3_100
import PlutusScripts.Batch6.Value.V3_110 qualified as V3_110
import PlutusTx.Prelude qualified as P

v3ValueScripts :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
v3ValueScripts =
  [ -- V3/1.1.0
    VersionedScript
      PlutusV3
      plc110
      "succeedingInsertNewCoinPolicyScript"
      V3_110.succeedingInsertNewCoinPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingInsertExistingCoinPolicyScript"
      V3_110.succeedingInsertExistingCoinPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingDeleteExistingCoinPolicyScript"
      V3_110.succeedingDeleteExistingCoinPolicy
    , VersionedScript
      PlutusV3
      plc110
      "succeedingDeleteMissingCoinPolicyScript"
      V3_110.succeedingDeleteMissingCoinPolicy
  ]
