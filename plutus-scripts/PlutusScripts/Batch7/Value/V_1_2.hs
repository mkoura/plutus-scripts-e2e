{-# OPTIONS_GHC -fplugin-opt Plinth.Plugin:target-version=1.2.0 #-}

module PlutusScripts.Batch7.Value.V_1_2 (allValueScripts) where

import Data.Text qualified as T
import Helpers.Envelopes (VersionedScript (VersionedScript), plc120)
import PlutusCore.Version (plcVersion120)
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage (PlutusV3, PlutusV4))
import PlutusScripts.Batch7.Value.Common (
  mkAssetCountPolicy,
  mkFailingFilterPolicy,
  mkFilterPolicy,
  mkPoliciesPolicy,
  mkSizePolicy,
 )
import PlutusScripts.Batch7.Value.Context (mkStateV3, mkStateV4)
import PlutusScripts.Batch7.Value.Scenarios (failureCases, filterCases, policyCases)
import PlutusTx (CompiledCode, compile, liftCode, unsafeApplyCode)
import PlutusTx.Builtins qualified as B
import PlutusTx.Prelude qualified as P

allValueScripts :: [VersionedScript (P.BuiltinData -> P.BuiltinUnit)]
allValueScripts = concatMap scripts [PlutusV3, PlutusV4]
 where
  scripts ll =
    map
      (uncurry (VersionedScript ll plc120))
      ( semanticPolicies
          ++ [
               ( "valueStateValidator"
               , if ll == PlutusV3
                   then $$(compile [||mkStateV3||])
                   else $$(compile [||mkStateV4||])
               )
             ]
      )

semanticPolicies :: [(T.Text, CompiledCode (P.BuiltinData -> P.BuiltinUnit))]
semanticPolicies =
  [ ( "succeedingValuePolicies" <> name <> "PolicyScript"
    , $$(compile [||mkPoliciesPolicy||])
        `unsafeApplyCode` liftCode plcVersion120 coins
        `unsafeApplyCode` liftCode plcVersion120 expected
    )
  | (name, coins, expected) <- policyCases
  ]
    ++ [("succeedingValueAssetCountPolicyScript", $$(compile [||mkAssetCountPolicy||]))]
    ++ [ ( "succeedingValue" <> op <> name <> "PolicyScript"
         , ( if keep
               then $$(compile [||mkFilterPolicy B.keepPolicies||])
               else $$(compile [||mkFilterPolicy B.dropPolicies||])
           )
             `unsafeApplyCode` liftCode plcVersion120 ids
             `unsafeApplyCode` liftCode plcVersion120 coins
             `unsafeApplyCode` liftCode plcVersion120 (if keep then kept else dropped)
         )
       | (name, ids, coins, kept, dropped) <- filterCases
       , (op, keep) <- [("Keep", True), ("Drop", False)]
       ]
    ++ [ ( "failingValue" <> op <> name <> "PolicyScript"
         , ( if keep
               then $$(compile [||mkFailingFilterPolicy B.keepPolicies||])
               else $$(compile [||mkFailingFilterPolicy B.dropPolicies||])
           )
             `unsafeApplyCode` liftCode plcVersion120 ids
             `unsafeApplyCode` liftCode plcVersion120 coins
         )
       | (name, ids, coins) <- failureCases
       , (op, keep) <- [("Keep", True), ("Drop", False)]
       ]
    ++ [ ( "succeedingValueSize" <> shape <> T.pack (show count) <> "PolicyScript"
         , $$(compile [||mkSizePolicy||])
             `unsafeApplyCode` liftCode plcVersion120 samePolicy
             `unsafeApplyCode` liftCode plcVersion120 count
         )
       | (shape, samePolicy) <- [("SamePolicy", True), ("MultiPolicy", False)]
       , count <- [1, 16, 64]
       ]
