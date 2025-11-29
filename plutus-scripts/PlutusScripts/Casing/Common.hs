{-| End to End cases for builtin constant casing.

Happy cases will cover all possible patterns of builtin constant casing for each types.
For example for builtin list, it will cover casing builtin list with single branch(only cons)
and with two branch(cons and nil).

Unhappy cases will cover cases when we expect builtin constant casing to fail. For example for
builtin list, it will cover casing empty builtin list with single branch which should fail
as it expects given list to be not empty. It will also cover incorrect number of branches.

Unhappy cases WILL NOT cover the cases when branches have incorrect types; for example,
the "uncons" branch for builtin list casing not being two nested lamAbs. We can safely skip this
tests because each branch taking correct number of arguments is indirectly checked by the happy
cases.
-}

module PlutusScripts.Casing.Common (
  caseIntegerHappy,
  caseIntegerUnhappyNoMatchOver,
  caseIntegerUnhappyNoMatchNegative,
  caseIntegerUnhappyNoBranches,
  caseListHappy,
  caseListUnhappyNoMatchNil,
  caseListUnhappyNoBranches,
  caseListUnhappyMoreBranches,
  casePairHappy,
  casePairUnhappyNoBranches,
  casePairUnhappyMoreBranches,
  caseBoolHappy,
  caseBoolUnhappyNoBranches,
  caseBoolUnhappyMoreBranches,
  caseUnitHappy,
  caseUnitUnhappyNoBranches,
  caseUnitUnhappyMoreBranches,
  asV3,
) where

import PlutusTx.Prelude qualified as P
import PlutusTx.Code (CompiledCodeIn (DeserializedCode))

import PlutusCore qualified as PLC
import PlutusCore.Annotation qualified as PLC
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Parser qualified as UPLC

import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3))

import Data.Set qualified as S
import Data.Text qualified as T

import Helpers.Envelopes (VersionedScript (VersionedScript), plc110)

parse :: T.Text -> UPLC.Term UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun PLC.SrcSpans
parse src =
  case PLC.runQuoteT $ UPLC.parseTerm src of
    Left parseErr -> error $ show parseErr
    Right t ->
      case PLC.runQuoteT $ UPLC.deBruijnTerm t of
        Left deBruijinErr -> error $ show deBruijinErr
        Right t' -> PLC.SrcSpans . S.singleton <$> t'

asV3
  :: T.Text
  -> T.Text
  -> VersionedScript (P.BuiltinData -> P.BuiltinUnit)
asV3 name src =
  let
    parsed :: UPLC.Term UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun PLC.SrcSpans
    parsed = UPLC.LamAbs mempty (UPLC.NamedDeBruijn "ctx" 0) $ parse src
  in
    VersionedScript
      PlutusV3
      plc110
      name
      (DeserializedCode (UPLC.Program mempty (UPLC.Version 1 1 0) parsed) Nothing mempty)

--------------------------------------------------------------------------------
-- Integer

caseIntegerHappy :: T.Text
caseIntegerHappy =
  T.intercalate "\n"
    [ "[(lam err"
    , "   (case (con integer 0)"
    , "     (case (con integer 1)"
    , "       [err (con string \"casing integer 1 went to wrong branch\")]"
    , "       (case (con integer 5)"
    , "         [err (con string \"casing integer 5 went to wrong branch\")]"
    , "         [err (con string \"casing integer 5 went to wrong branch\")]"
    , "         [err (con string \"casing integer 5 went to wrong branch\")]"
    , "         [err (con string \"casing integer 5 went to wrong branch\")]"
    , "         [err (con string \"casing integer 5 went to wrong branch\")]"
    , "         (con unit ())"
    , "         [err (con string \"casing integer 5 went to wrong branch\")]"
    , "         [err (con string \"casing integer 5 went to wrong branch\")]"
    , "         [err (con string \"casing integer 5 went to wrong branch\")]"
    , "       )"
    , "       [err (con string \"casing integer 1 went to wrong branch\")]"
    , "       [err (con string \"casing integer 1 went to wrong branch\")]"
    , "       [err (con string \"casing integer 1 went to wrong branch\")]"
    , "     )"
    , "     [err (con string \"casing integer 0 went to wrong branch\")]"
    , "     [err (con string \"casing integer 0 went to wrong branch\")]"
    , "     [err (con string \"casing integer 0 went to wrong branch\")]"
    , "   )"
    , " )"
    , " (lam str [(lam e (error)) [(builtin trace) str (con unit ())]])"
    , "]"
    ]

caseIntegerUnhappyNoMatchOver :: T.Text
caseIntegerUnhappyNoMatchOver =
  T.intercalate "\n"
    [ "(case (con integer 3)"
    , "  (con unit ())"
    , "  (con unit ())"
    , "  (con unit ())"
    , ")"
    ]

caseIntegerUnhappyNoMatchNegative :: T.Text
caseIntegerUnhappyNoMatchNegative =
  T.intercalate "\n"
    [ "(case (con integer -1)"
    , "  (con unit ())"
    , "  (con unit ())"
    , "  (con unit ())"
    , ")"
    ]

caseIntegerUnhappyNoBranches :: T.Text
caseIntegerUnhappyNoBranches =
  T.intercalate "\n"
    [ "(case (con integer 0)"
    , ")"
    ]

--------------------------------------------------------------------------------
-- List

-- We don't have any builtins that will compile into single branch list casing in Plinth.
-- So we have to hand-roll UPLC.

{- | Deconstruct given builtin list using casing on builtin list.
First casing matches [1, 2, 3] to 1 and [2, 3]. This casing checks it behaving correctly
when it's only given one branch(only cons). Second and third casing checks it behaving correctly
when it's given two branches(cons and nil). Last casing check it behaving correctly when given
empty list where it should take the second branch. IfThenElse on each "cons" branch check
casing behaving correctly--providing first element and the rest list.
-}
caseListHappy :: T.Text
caseListHappy =
  T.intercalate "\n"
    [ "[(lam err"
    , "  (case (con (list integer) [1, 2, 3])"
    , "     (lam x (lam xrest"
    , "       (force [ (force (builtin ifThenElse))"
    , "         [(builtin equalsInteger) (con integer 1) x]"
    , "         (delay"
    , "           (case xrest"
    , "             (lam y (lam yrest"
    , "               (force [ (force (builtin ifThenElse))"
    , "                 [(builtin equalsInteger) (con integer 2) y]"
    , "                 (delay"
    , "                   (case yrest"
    , "                     (lam z (lam zrest"
    , "                       (force [ (force (builtin ifThenElse))"
    , "                         [(builtin equalsInteger) (con integer 3) z]"
    , "                         (delay"
    , "                           (case zrest"
    , "                             [ (force (builtin trace))"
    , "                               (con string \"un-cons branch of list casing gave incorrect result: nil\")"
    , "                               (con unit ())"
    , "                             ]"
    , "                             [ (force (builtin trace))"
    , "                               (con string \"yay\")"
    , "                               (con unit ())"
    , "                             ]"
    , "                           )"
    , "                         )"
    , "                         (delay [err [ (force (builtin trace))"
    , "                           (con string \"un-cons branch of list casing gave incorrect result: 3\")"
    , "                           (con unit ())"
    , "                         ]])"
    , "                       ])"
    , "                     ))"
    , "                     [err [ (force (builtin trace))"
    , "                       (con string \"casing non-empty list went to wrong branch: 3\")"
    , "                       (con unit ())"
    , "                     ]]"
    , "                 ))"
    , "                 (delay [err [ (force (builtin trace))"
    , "                   (con string \"un-cons branch of list casing gave incorrect result: 2\")"
    , "                   (con unit ())"
    , "                 ]])"
    , "               ])"
    , "             ))"
    , "             [err [ (force (builtin trace))"
    , "               (con string \"casing non-empty list went to wrong branch: 2\")"
    , "               (con unit ())"
    , "             ]]"
    , "         ))"
    , "         (delay [err [ (force (builtin trace))"
    , "           (con string \"un-cons branch of list casing gave incorrect result: 1\")"
    , "           (con unit ())"
    , "         ]])"
    , "       ])"
    , "     ))"
    , "  ))"
    , "  (lam e (error))"
    , "]"
    ]

-- | Given empty array, but casing expects non-empty list.
caseListUnhappyNoMatchNil :: T.Text
caseListUnhappyNoMatchNil =
  T.intercalate "\n"
    [ "(case (con (list integer) [])"
    , "  (lam x (lam rest (con unit ())))"
    , ")"
    ]

-- | No branches given.
caseListUnhappyNoBranches :: T.Text
caseListUnhappyNoBranches =
  T.intercalate "\n"
    [ "(case (con (list integer) [1, 2, 3])"
    , ")"
    ]

-- | Extraneous branches given.
caseListUnhappyMoreBranches :: T.Text
caseListUnhappyMoreBranches =
  T.intercalate "\n"
    [ "(case (con (list integer) [1, 2, 3])"
    , "  (lam x (lam rest (con unit ())))"
    , "  (con unit ())"
    , "  (con unit ())"
    , ")"
    ]

--------------------------------------------------------------------------------
-- Pair

-- | Case given pair, check left value and right value are correct.
casePairHappy :: T.Text
casePairHappy =
  T.intercalate "\n"
    [ "[(lam err"
    , "   (case (con (pair integer integer) (4, 2))"
    , "     (lam l (lam r"
    , "       (force [ (force (builtin ifThenElse))"
    , "         [(builtin equalsInteger) (con integer 4) l]"
    , "         (delay"
    , "           (force [ (force (builtin ifThenElse))"
    , "             [(builtin equalsInteger) (con integer 2) r]"
    , "             (delay (con unit ()))"
    , "             (delay [err (con string \"casing pair gave incorrect result on right\")])"
    , "           ])"
    , "         )"
    , "         (delay [err (con string \"casing pair gave incorrect result on left\")])"
    , "       ])"
    , "     ))"
    , "   )"
    , " )"
    , " (lam str [(lam e (error)) [(builtin trace) str (con unit ())]])"
    , "]"
    ]

-- | No branches given.
casePairUnhappyNoBranches :: T.Text
casePairUnhappyNoBranches =
  T.intercalate "\n"
    [ "(case (con (pair integer integer) (4, 2))"
    , ")"
    ]

-- | Extraneous branches given.
casePairUnhappyMoreBranches :: T.Text
casePairUnhappyMoreBranches =
  T.intercalate "\n"
    [ "(case (con (pair integer integer) (4, 2))"
    , "  (lam l (lam r (con unit ())))"
    , "  (con unit ())"
    , ")"
    ]

--------------------------------------------------------------------------------
-- Bool

caseBoolHappy :: T.Text
caseBoolHappy =
  T.intercalate "\n"
    [ "[(lam err"
    , "   (case (con bool False)"
    , "     (case (con bool True)"
    , "       [err (con string \"casing True went to incorrect, False branch\")]"
    , "       (case (con bool False)"
    , "         (con unit ())"
    , "       )"
    , "     )"
    , "     [err (con string \"casing False went to incorrect, True branch\")]"
    , "   )"
    , " )"
    , " (lam str [(lam e (error)) [(builtin trace) str (con unit ())]])"
    , "]"
    ]

-- | No branches given.
caseBoolUnhappyNoBranches :: T.Text
caseBoolUnhappyNoBranches =
  T.intercalate "\n"
    [ "(case (con bool False)"
    , ")"
    ]

-- | Extraneous branches given.
caseBoolUnhappyMoreBranches :: T.Text
caseBoolUnhappyMoreBranches =
  T.intercalate "\n"
    [ "(case (con bool True)"
    , "  (con unit ())"
    , "  (con unit ())"
    , "  (con unit ())"
    , ")"
    ]

--------------------------------------------------------------------------------
-- Unit

caseUnitHappy :: T.Text
caseUnitHappy =
  T.intercalate "\n"
    [ "(case (con unit ())"
    , "  (con unit ())"
    , ")"
    ]

caseUnitUnhappyNoBranches :: T.Text
caseUnitUnhappyNoBranches =
  T.intercalate "\n"
    [ "(case (con unit ())"
    , ")"
    ]

caseUnitUnhappyMoreBranches :: T.Text
caseUnitUnhappyMoreBranches =
  T.intercalate "\n"
    [ "(case (con unit ())"
    , "  (con unit ())"
    , "  (con unit ())"
    , ")"
    ]
