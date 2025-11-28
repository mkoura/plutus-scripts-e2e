module PlutusScripts.Batch6.ExpModInteger (
  allExpModIntegerScripts,
  allExpModIntegerScriptGroups,
)
where

import Helpers.Envelopes (
  VersionedScript (VersionedScript),
  VersionedScriptGroup (VersionedScriptGroup),
  plc110,
 )
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))
import PlutusScripts.Batch6.ExpModInteger.V3_110 qualified as V3_110
import PlutusTx.Prelude qualified as P

allExpModIntegerScripts :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
allExpModIntegerScripts =
  [ VersionedScript
      PlutusV3
      plc110
      "succeedingExpModIntegerPolicyScript"
      V3_110.succeedingExpModIntegerPolicy
  , VersionedScript
      PlutusV3
      plc110
      "succeedingExpModIntegerInversePolicyScript"
      V3_110.succeedingExpModIntegerInversePolicy
  , VersionedScript
      PlutusV3
      plc110
      "succeedingExponentOnePolicyScript"
      V3_110.succeedingExponentOnePolicy
  ]

allExpModIntegerScriptGroups :: [VersionedScriptGroup (P.BuiltinData -> P.BuiltinUnit)]
allExpModIntegerScriptGroups =
  [ VersionedScriptGroup
      PlutusV3
      plc110
      "failingExpModIntegerScript"
      V3_110.failingExpModIntegerScriptGroup
  ]
