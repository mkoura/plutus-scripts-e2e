
module PlutusScripts.Helpers where

import Data.ByteString qualified as BS (ByteString)
import Data.Text qualified as T
import PlutusLedgerApi.Common.Versions (PlutusLedgerLanguage)
import PlutusLedgerApi.Envelope qualified as Envelope
import PlutusLedgerApi.V1.Bytes qualified as P (bytes, fromHex)
import PlutusLedgerApi.V1.Scripts (Datum (Datum), Redeemer (Redeemer))
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as P
import System.Directory (createDirectoryIfMissing)

-- | Treat string of hexadecimal bytes literally, without encoding. Useful for hashes.
bytesFromHex :: BS.ByteString -> BS.ByteString
bytesFromHex = P.bytes . fromEither . P.fromHex
  where
    fromEither (Left e) = error $ show e
    fromEither (Right b) = b

-- Helper to reduce clutter
hxs :: BS.ByteString -> P.BuiltinByteString
hxs = BI.toBuiltin . bytesFromHex

asRedeemer :: (PlutusTx.ToData a) => a -> Redeemer
asRedeemer = Redeemer . PlutusTx.dataToBuiltinData . PlutusTx.toData

asDatum :: (PlutusTx.ToData a) => a -> Datum
asDatum = Datum . PlutusTx.dataToBuiltinData . PlutusTx.toData

writeCompiledScript
  :: PlutusLedgerLanguage -> FilePath -> CompiledCode a -> IO ()
writeCompiledScript lang filename compiledCode = do
  let dir = "serialised-plutus-scripts"
      filePath = dir ++ "/" ++ filename ++ ".plutus"
  createDirectoryIfMissing True dir
  Envelope.writeCodeEnvelopeForVersion lang (T.pack "") compiledCode filePath
