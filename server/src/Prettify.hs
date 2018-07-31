{-# LANGUAGE OverloadedStrings #-}

module Prettify 
  ( DsMsg (..)
  , msgp
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec

prettifyIn :: Text -> Text
prettifyIn = undefined

prettifyOut :: Text -> Text
prettifyOut = undefined

data DsMsg = 
    DsQueryOk [Text] [Text]
  | DsQueryNo [Text]
  | DsCheck Text
  | DsResult Text

mkOk :: Text -> DsMsg
mkOk t = 
  case Text.splitOn ";" t of
    is:os:[] -> DsQueryOk (Text.splitOn "," is) (Text.splitOn "," os)

mkNo :: Text -> DsMsg
mkNo t = DsQueryNo (Text.splitOn "," t)

msgp :: Parsec Text () DsMsg
msgp = 
  string "DROIDSTAR:NG:" 
  >> choice 
       [ mkOk     <$> (string "QUERY:OK:" >> space >> getInput)
       , mkNo     <$> (string "Query:NO:" >> space >> getInput)
       , DsCheck  <$> (string "CHECK:" >> space >> getInput)
       , DsResult <$> (string "RESULT:" >> space >> getInput) ]

