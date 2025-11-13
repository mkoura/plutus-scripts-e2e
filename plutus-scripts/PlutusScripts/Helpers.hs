module PlutusScripts.Helpers where

import Data.ByteString qualified as BS (ByteString)
import PlutusLedgerApi.V1.Bytes qualified as P (bytes, fromHex)
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude qualified as P

-- | Treat string of hexadecimal bytes literally, without encoding. Useful for hashes.
bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = P.bytes . fromEither . P.fromHex
 where
  fromEither (Left e) = error $ show e
  fromEither (Right b) = b

-- Helper to reduce clutter
hxs :: BS.ByteString -> P.BuiltinByteString
hxs = BI.toBuiltin . bytesFromHex
