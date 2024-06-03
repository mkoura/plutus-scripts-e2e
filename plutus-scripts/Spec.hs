{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import System.IO (hSetEncoding, stdout, utf8)
import Spec.WriteScriptFiles (writeV3ScriptFiles)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  writeV3ScriptFiles
