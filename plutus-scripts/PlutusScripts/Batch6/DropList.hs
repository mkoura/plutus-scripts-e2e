module PlutusScripts.Batch6.DropList (
  allDropListScripts,
  allDropListScriptGroups,
)
where

import Helpers.Envelopes (
  VersionedScript (VersionedScript),
  VersionedScriptGroup (VersionedScriptGroup),
  plc110,
 )
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))
import PlutusScripts.Batch6.DropList.V3_110 qualified as V3_110
import PlutusTx.Prelude qualified as P

allDropListScripts :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
allDropListScripts =
  [ VersionedScript
      PlutusV3
      plc110
      "succeedingDropListPolicyScript"
      V3_110.succeedingDropListPolicy
  ]

allDropListScriptGroups :: [VersionedScriptGroup (P.BuiltinData -> P.BuiltinUnit)]
allDropListScriptGroups =
  [ VersionedScriptGroup
      PlutusV3
      plc110
      "expensiveDropListPolicyScript"
      V3_110.expensiveDropListScriptGroup
  ]
