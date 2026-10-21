module PlutusScripts.Casing.Casing (allCasingScripts, allCasingScriptsV1V2) where

import PlutusTx.Prelude qualified as P

import Helpers.Envelopes (VersionedScript, plc110, plc120)
import PlutusLedgerApi.Common.Versions (
  PlutusLedgerLanguage (PlutusV1, PlutusV2, PlutusV3, PlutusV4),
 )
import PlutusScripts.Casing.Common qualified as Casing

allCasingScripts :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
allCasingScripts =
  [ Casing.asVersioned PlutusV3 plc110 "caseIntegerHappy" Casing.caseIntegerHappy
  , Casing.asVersioned PlutusV3 plc110 "caseIntegerUnhappyNoMatchOver" Casing.caseIntegerUnhappyNoMatchOver
  , Casing.asVersioned PlutusV3 plc110 "caseIntegerUnhappyNoMatchNegative" Casing.caseIntegerUnhappyNoMatchNegative
  , Casing.asVersioned PlutusV3 plc110 "caseIntegerUnhappyNoBranches" Casing.caseIntegerUnhappyNoBranches
  , Casing.asVersioned PlutusV3 plc110 "caseListHappy" Casing.caseListHappy
  , Casing.asVersioned PlutusV3 plc110 "caseListUnhappyNoMatchNil" Casing.caseListUnhappyNoMatchNil
  , Casing.asVersioned PlutusV3 plc110 "caseListUnhappyNoBranches" Casing.caseListUnhappyNoBranches
  , Casing.asVersioned PlutusV3 plc110 "caseListUnhappyMoreBranches" Casing.caseListUnhappyMoreBranches
  , Casing.asVersioned PlutusV3 plc110 "casePairHappy" Casing.casePairHappy
  , Casing.asVersioned PlutusV3 plc110 "casePairUnhappyNoBranches" Casing.casePairUnhappyNoBranches
  , Casing.asVersioned PlutusV3 plc110 "casePairUnhappyMoreBranches" Casing.casePairUnhappyMoreBranches
  , Casing.asVersioned PlutusV3 plc110 "caseBoolHappy" Casing.caseBoolHappy
  , Casing.asVersioned PlutusV3 plc110 "caseBoolUnhappyNoBranches" Casing.caseBoolUnhappyNoBranches
  , Casing.asVersioned PlutusV3 plc110 "caseBoolUnhappyMoreBranches" Casing.caseBoolUnhappyMoreBranches
  , Casing.asVersioned PlutusV3 plc110 "caseUnitHappy" Casing.caseUnitHappy
  , Casing.asVersioned PlutusV3 plc110 "caseUnitUnhappyNoBranches" Casing.caseUnitUnhappyNoBranches
  , Casing.asVersioned PlutusV3 plc110 "caseUnitUnhappyMoreBranches" Casing.caseUnitUnhappyMoreBranches
  ]
    <> concatMap dataCasingScripts [PlutusV3, PlutusV4]

-- | PlutusV1 and PlutusV2 minting policies receive the redeemer and the script context.
allCasingScriptsV1V2 :: [VersionedScript (P.BuiltinData -> P.BuiltinData -> P.BuiltinUnit)]
allCasingScriptsV1V2 = concatMap dataCasingScripts [PlutusV1, PlutusV2]

-- | Casing on data is available in every ledger language, but only from Plutus Core 1.2.0.
dataCasingScripts :: PlutusLedgerLanguage -> [VersionedScript a]
dataCasingScripts ll =
  [ Casing.asVersioned ll plc120 "caseDataHappy" Casing.caseDataHappy
  , Casing.asVersioned ll plc120 "caseDataUnhappyNoMatchOver" Casing.caseDataUnhappyNoMatchOver
  , Casing.asVersioned ll plc120 "caseDataUnhappyNoMatchHuge" Casing.caseDataUnhappyNoMatchHuge
  , Casing.asVersioned ll plc120 "caseDataUnhappyNoBranches" Casing.caseDataUnhappyNoBranches
  , Casing.asVersioned ll plc120 "caseDataUnhappyInteger" Casing.caseDataUnhappyInteger
  , Casing.asVersioned ll plc120 "caseDataUnhappyByteString" Casing.caseDataUnhappyByteString
  , Casing.asVersioned ll plc120 "caseDataUnhappyList" Casing.caseDataUnhappyList
  , Casing.asVersioned ll plc120 "caseDataUnhappyMap" Casing.caseDataUnhappyMap
  , Casing.asVersioned
      ll
      plc110
      "caseDataUnhappyUnsupportedVersion"
      Casing.caseDataUnhappyUnsupportedVersion
  ]
