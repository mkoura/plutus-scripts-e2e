module PlutusScripts.Batch6.BLS12_381_MultiScalarMul (
  allMultiScalarMulScripts
)
where

import Helpers.Envelopes (
  VersionedScript (VersionedScript),
  plc110,
 )
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))
import PlutusScripts.Batch6.BLS12_381_MultiScalarMul.G1.V3_110 qualified as G1_V3_110
-- import PlutusScrips.Batch6.BLS12_381_MultiScalarMul.G2.V3_110 qualified as G2_V3_110
import PlutusTx.Prelude qualified as P

allMultiScalarMulScripts :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
allMultiScalarMulScripts =
  [ VersionedScript
      PlutusV3
      plc110
      "succeedingG1MultiScalarMulPolicyScript"
      G1_V3_110.succeedingMultiScalarMulPolicyScript
{-  , VersionedScript
      PlutusV3
      plc110
      "succeedingG1MultiScalarMulPolicyScript"
      G2_V3_110.succeedingMultiScalarMulPolicyScript
-}
  ]

