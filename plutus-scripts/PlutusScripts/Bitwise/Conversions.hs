-- editorconfig-checker-disable-file

module PlutusScripts.Bitwise.Conversions where

import PlutusLedgerApi.V1 qualified as PV1
import PlutusScripts.Helpers (hxs)

import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))

data ByteStringToIntegerParams = ByteStringToIntegerParams
  { bsByteOrder :: Bool
  , byteString  :: P.BuiltinByteString
  , expInteger  :: Integer
  }
PlutusTx.unstableMakeIsData ''ByteStringToIntegerParams

data IntegerToByteStringParams = IntegerToByteStringParams
  { intByteOrder  :: Bool
  , outputMinSize :: Integer
  , integer       :: Integer
  , expByteString :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''IntegerToByteStringParams

{-# INLINEABLE mkByteStringToIntegerPolicy #-}
mkByteStringToIntegerPolicy :: ByteStringToIntegerParams -> sc -> Bool
mkByteStringToIntegerPolicy ByteStringToIntegerParams{..} _sc = do
  let byteOrder = if bsByteOrder then BigEndian else LittleEndian
  let int = BI.byteStringToInteger byteOrder byteString
  int P.== expInteger

{-# INLINEABLE mkIntegerToByteStringPolicy #-}
mkIntegerToByteStringPolicy :: IntegerToByteStringParams -> sc -> Bool
mkIntegerToByteStringPolicy IntegerToByteStringParams{..} _sc = do
  let byteOrder = if intByteOrder then BigEndian else LittleEndian
  let bs = BI.integerToByteString byteOrder outputMinSize integer
  bs P.== expByteString

{-# INLINEABLE mkByteStringToIntegerRoundtripPolicySimple #-}
mkByteStringToIntegerRoundtripPolicySimple :: P.BuiltinData -> P.BuiltinData -> ()
mkByteStringToIntegerRoundtripPolicySimple r _sc = do
  let oInt = PV1.unsafeFromBuiltinData r :: Integer
      bs = BI.integerToByteString BigEndian 0 oInt
      intBE = BI.byteStringToInteger BigEndian bs
  if intBE P.== oInt then () else P.traceError "PT5"

{-# INLINEABLE mkByteStringToIntegerRoundtripPolicy #-}
mkByteStringToIntegerRoundtripPolicy :: P.BuiltinByteString -> sc -> Bool
mkByteStringToIntegerRoundtripPolicy bs _sc = do
  let intBE = BI.byteStringToInteger BigEndian bs
      bsBE = BI.integerToByteString BigEndian 0 intBE
      intLE = BI.byteStringToInteger LittleEndian bs
      bsLE = BI.integerToByteString LittleEndian 0 intLE
  bs P.== bsBE P.&& bs P.== bsLE

bsToIParams :: ByteStringToIntegerParams
bsToIParams =
  ByteStringToIntegerParams
    { bsByteOrder = True
    , byteString = hxs "deadbeef"
    , expInteger = 3735928559
    }

iToBsParams :: IntegerToByteStringParams
iToBsParams =
  IntegerToByteStringParams
    { intByteOrder = True
    , integer = 3735928559
    , outputMinSize = 0
    , expByteString = hxs "deadbeef"
    }

