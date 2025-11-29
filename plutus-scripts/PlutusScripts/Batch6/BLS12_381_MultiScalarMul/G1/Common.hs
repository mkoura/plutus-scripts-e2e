{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Simple end-to-end tests for the Plutus Core `BLS12_381_G1_MultiScalarMul`
builtin.  These are adapted from (some of) the `plutus-conformance` tests.
-}
module PlutusScripts.Batch6.BLS12_381_MultiScalarMul.G1.Common (mkSucceedingMultiScalarMulPolicy, succeedingMultiScalarMulParams)
where

import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)
import PlutusTx.Prelude qualified as P
import PlutusScripts.Helpers (hxs)

data Params = Params
  { scalars :: [Integer]
  , points  :: [BI.BuiltinByteString]
  , output :: BI.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

{-# INLINEABLE mkSucceedingMultiScalarMulPolicy #-}
mkSucceedingMultiScalarMulPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkSucceedingMultiScalarMulPolicy l _ctx = go l
 where
  go [] = BI.unitval
  go (Params{..} : rest) =
      if BI.bls12_381_G1_multiScalarMul scalars (fmap BI.bls12_381_G1_uncompress points)
         `BI.bls12_381_G1_equals` (BI.bls12_381_G1_uncompress output)
      then go rest
      else P.traceError "mkSucceedingMultiScalarMulPolicy"

-- mkG1point = BI.bls12_381_G1_uncompress . hxs

zero :: BI.BuiltinByteString
zero = hxs "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"


succeedingMultiScalarMulParams :: [Params]
succeedingMultiScalarMulParams =
  [ Params -- Empty list of scalars -> zero element;  infinite point in input.
    []  
    (fmap hxs
     [ "ab5d1d67b495361c3297c721cf3c9dc510fc5055bdf92fefc1e67d91a00a765520150428eb4c2fb01902d41d5ca62d85"
     , "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
     , "b1cdc04561c6bd93294fc05a3a27170ffdca767965cdd7f2ba6065d6805e9c3afbb64e68604ddce26401566a74b35fb4"
     , "8b9120083e12fe72d41e1488078a84a6629c2d90e00c238152b4cba64c20994196a1f4de9132f44597b566c80f17d64a"
     , "84a8c4b64514db1bedfab0baa53fbed998672626b4c715652583a504df9c44ad3fe106cb421694cc86b2f2371eacd28a"
     , "8b9120083e12fe72d41e1488078a84a6629c2d90e00c238152b4cba64c20994196a1f4de9132f44597b566c80f17d64a"
     ])
    zero
  , Params -- Empty list of scalars -> zero element;  no infinite points in input.
    []  
    (fmap hxs
    [ "89a1f2d2c350af95381b09f307e467ef4629aa3acd1c8eb6143d0a07bc072aa6c69635bcb3c83aab43a68e07f024f7b4"
    , "b9d49de64b4f0743f603e44697bea32d6b8065fa64efc77b435bcd3adea5af55a958e33034fe2cd13c99280f2987537a"
    , "93fd9bd18c19665db678479fb28a7aadce222986e894196bafd85d487d43a4bba22b66891d753f61e075c503b64f4166"
    , "85613b4871d8b2e0353c135c0b94f50a9966c424f6a8ee9d12c646392a22c51498317a50acd891589b358113acea4e62"
    , "8056aaf9b83cb4b3355ef95703832a03bcb9a6007bb1264908d4f27bc497aff4cacaed01c3e14c68914fb35327713bc5"
    , "876d7c92a343dcb531b4c0f6b8f69f68a669deceb2a157f3fc3275133dcd3f663da0750f6edcd7b8fa89a2572c44950d"
    ])
    zero
  , Params  -- Empty list of points -> zero element.
    [ -7843724524521392138901923801823923123123454352157
    , 34527457238
    , 941957124591735785278457826354751745783452348102412301239547134528453782352345235423452345234523452345234512341423412341234
    , -7
    , 78438238
    , 123
    ]
    []
    zero
  , Params  -- Both lists empty -> zero element.
    []
    []
    zero
  ]  
 -- Merge Yura's nix updates!!!
