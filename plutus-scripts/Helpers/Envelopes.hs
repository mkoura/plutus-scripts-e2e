module Helpers.Envelopes (
  PlutusCore (..),
  VersionedScript (..),
  VersionedScriptGroup (..),
  plc100,
  plc110,
  formatCoreVersion,
  formatPlutusVersion,
)
where

import Data.Text (Text)
import Data.Text qualified as T
import Helpers.ScriptUtils (ScriptGroup)
import PlutusCore.Default (DefaultFun, DefaultUni)
import PlutusLedgerApi.Common.Versions (
  PlutusLedgerLanguage (PlutusV1, PlutusV2, PlutusV3),
 )
import PlutusTx.Code (CompiledCode)

--------------------------------------------------------------------------------
-- Types -----------------------------------------------------------------------

-- | Plutus Core language version
data PlutusCore = PlutusCore Int Int Int
  deriving (Eq, Show)

-- | Common Plutus Core versions
plc100, plc110 :: PlutusCore
plc100 = PlutusCore 1 0 0
plc110 = PlutusCore 1 1 0

-- | A compiled script with version metadata
data VersionedScript a = VersionedScript
  { vsPlutusVersion :: PlutusLedgerLanguage
  , vsCoreVersion :: PlutusCore
  , vsScriptName :: Text
  , vsCompiledCode :: CompiledCode a
  }

-- | A script group with version metadata
data VersionedScriptGroup a = VersionedScriptGroup
  { vsgPlutusVersion :: PlutusLedgerLanguage
  , vsgCoreVersion :: PlutusCore
  , vsgBaseName :: Text
  , vsgScriptGroup :: ScriptGroup DefaultUni DefaultFun a
  }

--------------------------------------------------------------------------------
-- Formatting ------------------------------------------------------------------

-- | Format Plutus Core version as compact string (e.g., "100", "110")
formatCoreVersion :: PlutusCore -> Text
formatCoreVersion (PlutusCore major minor micro) =
  T.pack $ show major ++ show minor ++ show micro

-- | Format Plutus ledger version as string (e.g., "V1", "V2", "V3")
formatPlutusVersion :: PlutusLedgerLanguage -> Text
formatPlutusVersion PlutusV1 = "V1"
formatPlutusVersion PlutusV2 = "V2"
formatPlutusVersion PlutusV3 = "V3"
