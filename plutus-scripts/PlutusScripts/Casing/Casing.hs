module PlutusScripts.Casing.Casing (allCasingScripts) where

import PlutusTx.Prelude qualified as P

import Helpers.Envelopes (VersionedScript)
import PlutusScripts.Casing.Common qualified as Casing

allCasingScripts :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
allCasingScripts =
  [ Casing.asV3 "caseIntegerHappy" Casing.caseIntegerHappy
  , Casing.asV3 "caseIntegerUnhappyNoMatchOver" Casing.caseIntegerUnhappyNoMatchOver
  , Casing.asV3 "caseIntegerUnhappyNoMatchNegative" Casing.caseIntegerUnhappyNoMatchNegative
  , Casing.asV3 "caseIntegerUnhappyNoBranches" Casing.caseIntegerUnhappyNoBranches
  , Casing.asV3 "caseListHappy" Casing.caseListHappy
  , Casing.asV3 "caseListUnhappyNoMatchNil" Casing.caseListUnhappyNoMatchNil
  , Casing.asV3 "caseListUnhappyNoBranches" Casing.caseListUnhappyNoBranches
  , Casing.asV3 "caseListUnhappyMoreBranches" Casing.caseListUnhappyMoreBranches
  , Casing.asV3 "casePairHappy" Casing.casePairHappy
  , Casing.asV3 "casePairUnhappyNoBranches" Casing.casePairUnhappyNoBranches
  , Casing.asV3 "casePairUnhappyMoreBranches" Casing.casePairUnhappyMoreBranches
  , Casing.asV3 "caseBoolHappy" Casing.caseBoolHappy
  , Casing.asV3 "caseBoolUnhappyNoBranches" Casing.caseBoolUnhappyNoBranches
  , Casing.asV3 "caseBoolUnhappyMoreBranches" Casing.caseBoolUnhappyMoreBranches
  , Casing.asV3 "caseUnitHappy" Casing.caseUnitHappy
  , Casing.asV3 "caseUnitUnhappyNoBranches" Casing.caseUnitUnhappyNoBranches
  , Casing.asV3 "caseUnitUnhappyMoreBranches" Casing.caseUnitUnhappyMoreBranches
  ]
