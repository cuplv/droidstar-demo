{-# LANGUAGE OverloadedStrings #-}

module TypeState (mkTS) where

import Prelude hiding (FilePath)

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8,encodeUtf8)
import qualified Data.Text as Text
import Turtle
import Data.Digest.Pure.SHA (sha1,showDigest)
import qualified Data.ByteString.Lazy as BL

tsDir = fromText "typestates"

-- | Perform a SHA1 on 'Text', returning the digest as 'Text'
sha1Text :: Text -> Text
sha1Text = 
  Text.pack 
  . showDigest 
  . sha1 
  . BL.fromStrict 
  . encodeUtf8

-- | Create a typestate image and return its relative URI
mkTS :: Text -> IO Text
mkTS ts = do
  let gv = tsDir <> fromText (sha1Text ts <> ".gv")
      img = tsDir <> fromText (sha1Text ts <> ".png")
  mktree tsDir
  writeTextFile gv ts
  proc "dot" ["-Tpng","-o" <> format fp img,format fp gv] empty
  return (format fp img)
