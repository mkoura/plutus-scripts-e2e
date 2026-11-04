-- | Inputs and independent complete expected Values for the semantic policies.
module PlutusScripts.Batch7.Value.Scenarios (
  policyCases,
  filterCases,
  failureCases,
) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import PlutusTx.Builtins (BuiltinByteString, toBuiltin)

type Coin = (BuiltinByteString, BuiltinByteString, Integer)

p, q, absent, overlong :: BuiltinByteString
p = toBuiltin (BS.replicate 28 0x11)
q = toBuiltin (BS.replicate 28 0x22)
absent = toBuiltin (BS.replicate 28 0x33)
overlong = toBuiltin (BS.replicate 28 0x11 <> BS.replicate 5 0x44)

ada, px, py, qx :: Coin
ada = ("", "", 100)
px = (p, "x", 7)
py = (p, "y", -3)
qx = (q, "x", 9)

sample :: [Coin]
sample = [qx, py, ada, px]

policyCases :: [(Text, [Coin], [BuiltinByteString])]
policyCases =
  [ ("Empty", [], [])
  , ("Ada", [ada], [""])
  , ("Sorted", sample, ["", p, q])
  ]

-- | Name, IDs, input, expected keep result, expected drop result.
filterCases :: [(Text, [BuiltinByteString], [Coin], [Coin], [Coin])]
filterCases =
  [ ("EmptyValue", [p, ""], [], [], [])
  , ("EmptyIds", [], sample, [], sample)
  , ("All", [q, "", p], sample, sample, [])
  , ("Subset", [p], sample, [px, py], [ada, qx])
  , ("Ada", [""], sample, [ada], [px, py, qx])
  ,
    ( "DuplicateAbsentOverlong"
    , [q, absent, q, overlong, toBuiltin (BS.replicate 32 0x55)]
    , sample
    , [qx]
    , [ada, px, py]
    )
  , ("Duplicate256", replicate 256 p, sample, [px, py], [ada, qx])
  , ("Absent256Empty", replicate 256 absent, [], [], [])
  , ("Overlong256Empty", replicate 256 overlong, [], [], [])
  ,
    ( "Limit256"
    , [p, p, absent, overlong] ++ replicate 252 absent
    , sample
    , [px, py]
    , [ada, qx]
    )
  ]

-- | All input entries count, including duplicates and invalid/absent IDs.
failureCases :: [(Text, [BuiltinByteString], [Coin])]
failureCases =
  [ ("Duplicate257", replicate 257 p, sample)
  , ("Absent257Empty", replicate 257 absent, [])
  , ("Overlong257Empty", replicate 257 overlong, [])
  ]
