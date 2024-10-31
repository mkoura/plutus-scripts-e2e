{-| Simple end-to-end tests for the Plutus Core `andByteString`, `orByteString`,
and `xorByteString` builtins.  These are adapted from the `plutus-conformance`
tests. -}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

module PlutusScripts.Bitwise.Logical (
  mkAndByteStringPolicy,
  mkOrByteStringPolicy,
  mkXorByteStringPolicy,
  succeedingAndByteStringParams,
  succeedingOrByteStringParams,
  succeedingXorByteStringParams
  )
where

import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Builtins.Internal qualified as BI (unitval)
import PlutusTx.Prelude qualified as P

import PlutusScripts.Helpers (hxs)

-- Logical bitwise operations

-- Parameters for andByteString/orByteString/xorByteString
data Params = Params
  { extend :: Bool --- Extend or truncate if inputs are of different lengths?
  , input1 :: P.BuiltinByteString
  , input2 :: P.BuiltinByteString
  , output :: P.BuiltinByteString
  }
PlutusTx.unstableMakeIsData ''Params
PlutusTx.makeLift ''Params

{-# INLINEABLE mkAndByteStringPolicy #-}
mkAndByteStringPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkAndByteStringPolicy l _ctx = go l
  where go [] = BI.unitval
        go (Params{..}:rest) =
          if BI.andByteString extend input1 input2 P.== output
          then go rest
          else P.traceError "mkAndByteStringPolicy"

{-# INLINEABLE mkOrByteStringPolicy #-}
mkOrByteStringPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkOrByteStringPolicy l _ctx = go l
  where go [] = BI.unitval
        go (Params{..}:rest) =
          if BI.orByteString extend input1 input2 P.== output
          then go rest
          else P.traceError "mkOrByteStringPolicy"

{-# INLINEABLE mkXorByteStringPolicy #-}
mkXorByteStringPolicy :: [Params] -> P.BuiltinData -> P.BuiltinUnit
mkXorByteStringPolicy l _ctx = go l
  where go [] = BI.unitval
        go (Params{..}:rest) =
          if BI.xorByteString extend input1 input2 P.== output
          then go rest
          else P.traceError "mkXorByteStringPolicy"

-- Succeeding inputs; the `andByteString` function can never fail.
succeedingAndByteStringParams :: [Params]
succeedingAndByteStringParams =
 [ Params
   { extend = False
   , input1 = hxs ""
   , input2 = hxs "ff"
   , output = hxs ""
   }
 , Params
   { extend = False
   , input1 = hxs "ff"
   , input2 = hxs ""
   , output = hxs ""
   }
 , Params
   { extend = False
   , input1 = hxs "ff"
   , input2 = hxs "00"
   , output = hxs "00"
   }
 , Params
   { extend = False
   , input1 = hxs "00"
   , input2 = hxs "ff"
   , output = hxs "00"
   }
 , Params
   { extend = False
   , input1 = hxs "4f00"
   , input2 = hxs "f4"
   , output = hxs "44"
   }
 , Params
   { extend = True
   , input1 = hxs ""
   , input2 = hxs "ff"
   , output = hxs "ff"
   }
 , Params
   { extend = True
   , input1 = hxs "ff"
   , input2 = hxs ""
   , output = hxs "ff"
   }
 , Params
   { extend = True
   , input1 = hxs "ff"
   , input2 = hxs "00"
   , output = hxs "00"
   }
 , Params
   { extend = True
   , input1 = hxs "00"
   , input2 = hxs "ff"
   , output = hxs "00"
   }
 , Params
   { extend = True
   , input1 = hxs "4f00"
   , input2 = hxs "f4"
   , output = hxs "4400"
   }
 , Params
   { extend = False
   , input1 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
   , input2 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
   , output = hxs "13808014808080189900422a0288ac0203640002540830091400164401020021093f210424001cc5508210"
   }
 , Params
   { extend = True
   , input1 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
   , input2 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
   , output = hxs "13808014808080189900422a0288ac0203640002540830091400164401020021093f210424001cc55082101b55b625553af3"
   }
 , Params
   { extend = False
   , input1 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
   , input2 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
   , output = hxs "13808014808080189900422a0288ac0203640002540830091400164401020021093f210424001cc5508210"
   }
 , Params
   { extend = True
   , input1 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
   , input2 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
   , output = hxs "13808014808080189900422a0288ac0203640002540830091400164401020021093f210424001cc55082101b55b625553af3"
   }
 ]

-- Succeeding inputs; the `orByteString` function can never fail.
succeedingOrByteStringParams :: [Params]
succeedingOrByteStringParams =
  [ Params
    { extend = False
    , input1 = hxs ""
    , input2 = hxs "ff"
    , output = hxs ""
    }
  , Params
    { extend = False
    , input1 = hxs "ff"
    , input2 = hxs ""
    , output = hxs ""
    }
  , Params
    { extend = False
    , input1 = hxs "ff"
    , input2 = hxs "00"
    , output = hxs "ff"
    }
  , Params
    { extend = False
    , input1 = hxs "00"
    , input2 = hxs "ff"
    , output = hxs "ff"
    }
  , Params
    { extend = False
    , input1 = hxs "4f00"
    , input2 = hxs "f4"
    , output = hxs "ff"
    }
  , Params
    { extend = True
    , input1 = hxs ""
    , input2 = hxs "ff"
    , output = hxs "ff"
    }
  , Params
    { extend = True
    , input1 = hxs "ff"
    , input2 = hxs ""
    , output = hxs "ff"
    }
  , Params
    { extend = True
    , input1 = hxs "ff"
    , input2 = hxs "00"
    , output = hxs "ff"
    }
  , Params
    { extend = True
    , input1 = hxs "00"
    , input2 = hxs "ff"
    , output = hxs "ff"
    }
  , Params
    { extend = True
    , input1 = hxs "4f00"
    , input2 = hxs "f4"
    , output = hxs "ff00"
    }
  , Params
    { extend = False
    , input1 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , input2 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
    , output = hxs "fbdef6de9da7d1bfbde8db3b3bbebf6ecffdc1dffc6bf3dd7f613e57df8fd7ffdd7fefb6e7be5ecfdff7f8"
    }
  , Params
    { extend = True
    , input1 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , input2 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
    , output = hxs "fbdef6de9da7d1bfbde8db3b3bbebf6ecffdc1dffc6bf3dd7f613e57df8fd7ffdd7fefb6e7be5ecfdff7f81b55b625553af3"
    }
  , Params
    { extend = False
    , input1 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
    , input2 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , output = hxs "fbdef6de9da7d1bfbde8db3b3bbebf6ecffdc1dffc6bf3dd7f613e57df8fd7ffdd7fefb6e7be5ecfdff7f8"
    }
  , Params
    { extend = True
    , input1 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
    , input2 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , output = hxs "fbdef6de9da7d1bfbde8db3b3bbebf6ecffdc1dffc6bf3dd7f613e57df8fd7ffdd7fefb6e7be5ecfdff7f81b55b625553af3"
    }
  ]

-- Succeeding inputs; the `xorByteString` function can never fail.
succeedingXorByteStringParams :: [Params]
succeedingXorByteStringParams =
  [ Params
    { extend = False
    , input1 = hxs ""
    , input2 = hxs "ff"
    , output = hxs ""
    }
  , Params
    { extend = False
    , input1 = hxs "ff"
    , input2 = hxs ""
    , output = hxs ""
    }
  , Params
    { extend = False
    , input1 = hxs "ff"
    , input2 = hxs "00"
    , output = hxs "ff"
    }
  , Params
    { extend = False
    , input1 = hxs "00"
    , input2 = hxs "ff"
    , output = hxs "ff"
    }
  , Params
    { extend = False
    , input1 = hxs "4f00"
    , input2 = hxs "f4"
    , output = hxs "bb"
    }
  , Params
    { extend = True
    , input1 = hxs ""
    , input2 = hxs "ff"
    , output = hxs "ff"
    }
  , Params
    { extend = True
    , input1 = hxs "ff"
    , input2 = hxs ""
    , output = hxs "ff"
    }
  , Params
    { extend = True
    , input1 = hxs "ff"
    , input2 = hxs "00"
    , output = hxs "ff"
    }
  , Params
    { extend = True
    , input1 = hxs "00"
    , input2 = hxs "ff"
    , output = hxs "ff"
    }
  , Params
    { extend = True
    , input1 = hxs "4f00"
    , input2 = hxs "f4"
    , output = hxs "bb00"
    }
  , Params
    { extend = False
    , input1 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , input2 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
    , output = hxs "e85e76ca1d2751a724e899113936136ccc99c1dda863c3d46b612813de8dd7ded440ceb2c3be420a8f75e8"
    }
  , Params
    { extend = True
    , input1 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , input2 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
    , output = hxs "e85e76ca1d2751a724e899113936136ccc99c1dda863c3d46b612813de8dd7ded440ceb2c3be420a8f75e81b55b625553af3"
    }
  , Params
    { extend = False
    , input1 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
    , input2 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , output = hxs "e85e76ca1d2751a724e899113936136ccc99c1dda863c3d46b612813de8dd7ded440ceb2c3be420a8f75e8"
    }
  , Params
    { extend = True
    , input1 = hxs "db9c861c98a3d19cb928c22a32aaae0a4f740113dc48734d3c001657cb8fd2b9497faf16a40c1ecdd7d6581b55b625553af3"
    , input2 = hxs "33c2f0d68584803b9dc05b3b0b9cbd6683edc0ce742bb09957613e44150205679d3f61a467b25cc758a3b0"
    , output = hxs "e85e76ca1d2751a724e899113936136ccc99c1dda863c3d46b612813de8dd7ded440ceb2c3be420a8f75e81b55b625553af3"
    }
  ]
