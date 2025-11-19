-- editorconfig-checker-disable-file

module PlutusScripts.Array.Common (
  mkIndexArrayPolicy,
  mkLengthOfArrayPolicy,
  mkListToArrayPolicy,
)
where

import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)
import PlutusTx.Prelude qualified as P

{-# INLINEABLE mkIndexArrayPolicy #-}
mkIndexArrayPolicy :: P.BuiltinData -> P.BuiltinUnit
mkIndexArrayPolicy _ctx =
  let list = P.toOpaque [1 :: Integer, 2, 3, 4, 5]
      arr = BI.listToArray list
      element = BI.indexArray arr 2
  in if element P.== 3
     then BI.unitval
     else P.traceError "mkIndexArrayPolicy"

{-# INLINEABLE mkLengthOfArrayPolicy #-}
mkLengthOfArrayPolicy :: P.BuiltinData -> P.BuiltinUnit
mkLengthOfArrayPolicy _ctx =
  let list = P.toOpaque [10 :: Integer, 20, 30]
      arr = BI.listToArray list
      len = BI.lengthOfArray arr
  in if len P.== 3
     then BI.unitval
     else P.traceError "mkLengthOfArrayPolicy"

{-# INLINEABLE mkListToArrayPolicy #-}
mkListToArrayPolicy :: P.BuiltinData -> P.BuiltinUnit
mkListToArrayPolicy _ctx =
  let list = P.toOpaque [100 :: Integer, 200, 300, 400]
      arr = BI.listToArray list
      -- Verify conversion by checking array properties
      len = BI.lengthOfArray arr
      firstElem = BI.indexArray arr 0
      lastElem = BI.indexArray arr 3
  in if len P.== 4 P.&& firstElem P.== 100 P.&& lastElem P.== 400
     then BI.unitval
     else P.traceError "mkListToArrayPolicy"
