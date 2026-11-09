-- | Versioned multiIndexArray policies for PlutusV3 and PlutusV4.
module PlutusScripts.Batch7.MultiIndexArray (allMultiIndexArrayScripts) where

import Helpers.Envelopes (VersionedScript)
import PlutusScripts.Batch7.MultiIndexArray.V110 qualified as V110
import PlutusScripts.Batch7.MultiIndexArray.V120 qualified as V120
import PlutusTx.Prelude qualified as P

-- | Combine both Plutus Core versions, each covering both ledger languages.
allMultiIndexArrayScripts :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
allMultiIndexArrayScripts =
  V110.allMultiIndexArrayScripts <> V120.allMultiIndexArrayScripts
