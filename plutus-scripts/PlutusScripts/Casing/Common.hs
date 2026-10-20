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

Casing on builtin data is only available from the Dijkstra hard fork onwards and only for
Plutus Core 1.2.0 and later, for every ledger language. Hence the data cases are meant to be
built as Plutus Core 1.2.0 scripts, except for 'caseDataUnhappyUnsupportedVersion' which is meant
to be built as a Plutus Core 1.1.0 script, where casing on data is expected to fail.
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
  caseDataHappy,
  caseDataUnhappyNoMatchOver,
  caseDataUnhappyNoMatchHuge,
  caseDataUnhappyNoBranches,
  caseDataUnhappyInteger,
  caseDataUnhappyByteString,
  caseDataUnhappyList,
  caseDataUnhappyMap,
  caseDataUnhappyUnsupportedVersion,
  asVersioned,
) where

import PlutusTx.Code (CompiledCodeIn (DeserializedCode))

import PlutusCore qualified as PLC
import PlutusCore.Annotation qualified as PLC
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Parser qualified as UPLC

import PlutusLedgerApi.Common.Versions (
  PlutusLedgerLanguage (PlutusV1, PlutusV2, PlutusV3, PlutusV4),
 )

import Data.Set qualified as S
import Data.Text qualified as T

import Helpers.Envelopes (PlutusCore (PlutusCore), VersionedScript (VersionedScript))

parse :: T.Text -> UPLC.Term UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun PLC.SrcSpans
parse src =
  case PLC.runQuoteT $ UPLC.parseTerm src of
    Left parseErr -> error $ show parseErr
    Right t ->
      case PLC.runQuoteT $ UPLC.deBruijnTerm t of
        Left deBruijinErr -> error $ show deBruijinErr
        Right t' -> PLC.SrcSpans . S.singleton <$> t'

{-| Make a minting policy of given ledger language and Plutus Core version out of given UPLC term.
PlutusV1 and PlutusV2 minting policies receive the redeemer and the script context, while PlutusV3
and PlutusV4 minting policies receive only the script context. Arguments are ignored.
-}
asVersioned
  :: PlutusLedgerLanguage
  -> PlutusCore
  -> T.Text
  -> T.Text
  -> VersionedScript a
asVersioned ll plcVersion@(PlutusCore major minor patch) name src =
  let
    args :: [T.Text]
    args = case ll of
      PlutusV1 -> ["redeemer", "ctx"]
      PlutusV2 -> ["redeemer", "ctx"]
      PlutusV3 -> ["ctx"]
      PlutusV4 -> ["ctx"]

    parsed :: UPLC.Term UPLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun PLC.SrcSpans
    parsed = foldr (\arg -> UPLC.LamAbs mempty (UPLC.NamedDeBruijn arg 0)) (parse src) args

    version :: UPLC.Version
    version = UPLC.Version (fromIntegral major) (fromIntegral minor) (fromIntegral patch)
  in
    VersionedScript
      ll
      plcVersion
      name
      (DeserializedCode (UPLC.Program mempty version parsed) Nothing mempty)

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

--------------------------------------------------------------------------------
-- Data

-- There is no case for a negative tag, since such constructor can't exist at runtime: it can't be
-- deserialised from a constant and @constrData@ fails on tags that don't fit into 'Word64'.

{-| @caseConstrStep desc scrutinee tag branches fields next@ cases on @scrutinee@ using @branches@
branches, expecting the branch at index @tag@ to be selected and to receive @fields@. Every other
branch fails. When the selected branch received the expected fields, continue with @next@.
-}
caseConstrStep :: T.Text -> T.Text -> Int -> Int -> T.Text -> [T.Text] -> [T.Text]
caseConstrStep desc scrutinee tag branches fields next =
  [ "[(lam wrong"
  , "  (case " <> scrutinee
  ]
    <> replicate tag "    wrong"
    <> [ "    (lam fields"
       , "      (force [ (force (builtin ifThenElse))"
       , "        [(builtin equalsData) (con data (List " <> fields <> ")) [(builtin listData) fields]]"
       , "        (delay"
       ]
    <> map ("          " <>) next
    <> [ "        )"
       , "        (delay [err (con string \"casing " <> desc <> " gave incorrect fields\")])"
       , "      ])"
       , "    )"
       ]
    <> replicate (branches - tag - 1) "    wrong"
    <> [ "  )"
       , " )"
       , " (lam fields [err (con string \"casing " <> desc <> " went to wrong branch\")])"
       , "]"
       ]

{-| Case on a constructor nested in the fields of another constructor, checking that the fields
given to the branch are proper data values which can be cased on again.
-}
caseNestedConstrStep :: [T.Text] -> [T.Text]
caseNestedConstrStep next =
  [ "(case (con data (Constr 0 [Constr 1 [I 5]]))"
  , "  (lam fields"
  , "    (case fields"
  , "      (lam inner (lam rest"
  , "        (case rest"
  , "          (lam x (lam xs"
  , "            [err (con string \"casing outer Constr 0 gave incorrect fields: too many\")]"
  , "          ))"
  ]
    <> map ("          " <>) (caseConstrStep "nested Constr 1" "inner" 1 2 "[I 5]" next)
    <> [ "        )"
       , "      ))"
       , "      [err (con string \"casing outer Constr 0 gave incorrect fields: nil\")]"
       , "    )"
       , "  )"
       , ")"
       ]

{- | Case on various constructors, checking the right branch is selected and it receives the
constructor fields. This covers:

* Constructor with no fields and a single branch.
* Extraneous branches, which are allowed as long as the tag is in bounds.
* Tags on the boundaries of the different CBOR encodings of constructor tags: 6 and 7
  (CBOR tags 121-127 and 1280-1400) as well as 127 and 128 (CBOR tags 1280-1400 and 102).
* Constructor built at runtime with @constrData@.
* Constructor nested in the fields of another constructor.
-}
caseDataHappy :: T.Text
caseDataHappy =
  T.intercalate "\n" $
    [ "[(lam err"
    ]
      <> map ("   " <>) (foldr ($) ["(con unit ())"] steps)
      <> [ " )"
         , " (lam str [(lam e (error)) [(force (builtin trace)) str (con unit ())]])"
         , "]"
         ]
  where
    steps :: [[T.Text] -> [T.Text]]
    steps =
      [ caseConstrStep "Constr 0" "(con data (Constr 0 []))" 0 1 "[]"
      , caseConstrStep
          "Constr 1"
          "(con data (Constr 1 [I 42, B #abcd, List [I 7], Map [(I 1, B #00)], Constr 3 []]))"
          1
          3
          "[I 42, B #abcd, List [I 7], Map [(I 1, B #00)], Constr 3 []]"
      , caseConstrStep "Constr 6" "(con data (Constr 6 [I 6]))" 6 7 "[I 6]"
      , caseConstrStep "Constr 7" "(con data (Constr 7 [I 7]))" 7 8 "[I 7]"
      , caseConstrStep "Constr 127" "(con data (Constr 127 [I 127]))" 127 128 "[I 127]"
      , caseConstrStep "Constr 128" "(con data (Constr 128 [I 128]))" 128 129 "[I 128]"
      , caseConstrStep
          "runtime Constr 2"
          "[(builtin constrData) (con integer 2) (con (list data) [I 1, B #ff])]"
          2
          3
          "[I 1, B #ff]"
      , caseNestedConstrStep
      ]

-- | Tag is greater than the index of the last branch.
caseDataUnhappyNoMatchOver :: T.Text
caseDataUnhappyNoMatchOver =
  T.intercalate "\n"
    [ "(case (con data (Constr 3 []))"
    , "  (lam fields (con unit ()))"
    , "  (lam fields (con unit ()))"
    , "  (lam fields (con unit ()))"
    , ")"
    ]

{- | Largest tag a constructor can have, it doesn't fit into a signed 64-bit integer. Such constant
is serialised using the CBOR tag 102.
-}
caseDataUnhappyNoMatchHuge :: T.Text
caseDataUnhappyNoMatchHuge =
  T.intercalate "\n"
    [ "(case (con data (Constr 18446744073709551615 []))"
    , "  (lam fields (con unit ()))"
    , ")"
    ]

-- | No branches given.
caseDataUnhappyNoBranches :: T.Text
caseDataUnhappyNoBranches =
  T.intercalate "\n"
    [ "(case (con data (Constr 0 []))"
    , ")"
    ]

-- | Casing only supports constructors, not integers.
caseDataUnhappyInteger :: T.Text
caseDataUnhappyInteger =
  T.intercalate "\n"
    [ "(case (con data (I 0))"
    , "  (lam fields (con unit ()))"
    , ")"
    ]

-- | Casing only supports constructors, not bytestrings.
caseDataUnhappyByteString :: T.Text
caseDataUnhappyByteString =
  T.intercalate "\n"
    [ "(case (con data (B #00))"
    , "  (lam fields (con unit ()))"
    , ")"
    ]

-- | Casing only supports constructors, not lists.
caseDataUnhappyList :: T.Text
caseDataUnhappyList =
  T.intercalate "\n"
    [ "(case (con data (List [I 0]))"
    , "  (lam fields (con unit ()))"
    , ")"
    ]

-- | Casing only supports constructors, not maps.
caseDataUnhappyMap :: T.Text
caseDataUnhappyMap =
  T.intercalate "\n"
    [ "(case (con data (Map [(I 0, I 0)]))"
    , "  (lam fields (con unit ()))"
    , ")"
    ]

{- | Well-formed casing on data, meant to be built as a Plutus Core 1.1.0 script where casing on
data is not available.
-}
caseDataUnhappyUnsupportedVersion :: T.Text
caseDataUnhappyUnsupportedVersion =
  T.intercalate "\n"
    [ "(case (con data (Constr 0 []))"
    , "  (lam fields (con unit ()))"
    , ")"
    ]
