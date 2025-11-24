module PlutusScripts.Batch6.Array (allArrayScripts) where

import Helpers.Envelopes (VersionedScript (VersionedScript), plc100, plc110)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV1, PlutusV2, PlutusV3))
import PlutusScripts.Batch6.Array.V1_100 qualified as V1_100
import PlutusScripts.Batch6.Array.V1_110 qualified as V1_110
import PlutusScripts.Batch6.Array.V2_100 qualified as V2_100
import PlutusScripts.Batch6.Array.V2_110 qualified as V2_110
import PlutusScripts.Batch6.Array.V3_100 qualified as V3_100
import PlutusScripts.Batch6.Array.V3_110 qualified as V3_110
import PlutusTx.Prelude qualified as P

allArrayScripts :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
allArrayScripts =
  [ -- V1/1.0.0
    VersionedScript PlutusV1 plc100 "succeedingIndexArrayPolicyScript"
      V1_100.succeedingIndexArrayPolicy
  , VersionedScript PlutusV1 plc100 "succeedingLengthOfArrayPolicyScript"
      V1_100.succeedingLengthOfArrayPolicy
  , VersionedScript PlutusV1 plc100 "succeedingListToArrayPolicyScript"
      V1_100.succeedingListToArrayPolicy
    -- V1/1.1.0
  , VersionedScript PlutusV1 plc110 "succeedingIndexArrayPolicyScript"
      V1_110.succeedingIndexArrayPolicy
  , VersionedScript PlutusV1 plc110 "succeedingLengthOfArrayPolicyScript"
      V1_110.succeedingLengthOfArrayPolicy
  , VersionedScript PlutusV1 plc110 "succeedingListToArrayPolicyScript"
      V1_110.succeedingListToArrayPolicy
    -- V2/1.0.0
  , VersionedScript PlutusV2 plc100 "succeedingIndexArrayPolicyScript"
      V2_100.succeedingIndexArrayPolicy
  , VersionedScript PlutusV2 plc100 "succeedingLengthOfArrayPolicyScript"
      V2_100.succeedingLengthOfArrayPolicy
  , VersionedScript PlutusV2 plc100 "succeedingListToArrayPolicyScript"
      V2_100.succeedingListToArrayPolicy
    -- V2/1.1.0
  , VersionedScript PlutusV2 plc110 "succeedingIndexArrayPolicyScript"
      V2_110.succeedingIndexArrayPolicy
  , VersionedScript PlutusV2 plc110 "succeedingLengthOfArrayPolicyScript"
      V2_110.succeedingLengthOfArrayPolicy
  , VersionedScript PlutusV2 plc110 "succeedingListToArrayPolicyScript"
      V2_110.succeedingListToArrayPolicy
    -- V3/1.0.0
  , VersionedScript PlutusV3 plc100 "succeedingIndexArrayPolicyScript"
      V3_100.succeedingIndexArrayPolicy
  , VersionedScript PlutusV3 plc100 "succeedingLengthOfArrayPolicyScript"
      V3_100.succeedingLengthOfArrayPolicy
  , VersionedScript PlutusV3 plc100 "succeedingListToArrayPolicyScript"
      V3_100.succeedingListToArrayPolicy
    -- V3/1.1.0
  , VersionedScript PlutusV3 plc110 "succeedingIndexArrayPolicyScript"
      V3_110.succeedingIndexArrayPolicy
  , VersionedScript PlutusV3 plc110 "succeedingLengthOfArrayPolicyScript"
      V3_110.succeedingLengthOfArrayPolicy
  , VersionedScript PlutusV3 plc110 "succeedingListToArrayPolicyScript"
      V3_110.succeedingListToArrayPolicy
  ]
