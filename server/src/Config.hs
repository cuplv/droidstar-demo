{-# LANGUAGE OverloadedStrings #-}

module Config where

import Prelude hiding (FilePath)

import Turtle

data Config = Config { emuAddr :: Maybe String
                     , apksDir :: FilePath
                     , initDelay :: Int }

-- cfg = Config (Just "172.17.0.2") (fromText "..") 0
cfg = Config Nothing (fromText "/root/apks") 120
