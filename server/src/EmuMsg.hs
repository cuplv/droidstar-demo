{-# LANGUAGE OverloadedStrings #-}

module EmuMsg (EmuMsg (..), msgp) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec
import Control.Monad.Trans

import TypeState


-- | Messages that DroidStar outputs
data EmuMsg = 
    DsQueryOk [Text] [Text]
  | DsQueryNo [Text]
  | DsCheck Text
  | DsCex [Text]
  | DsResult Text

mkOk :: Text -> EmuMsg
mkOk t = 
  case Text.splitOn ";" t of
    is:os:[] -> DsQueryOk (Text.splitOn "," is) (Text.splitOn "," os)

mkNo :: Text -> EmuMsg
mkNo t = DsQueryNo (Text.splitOn "," t)

mkCex :: Text -> EmuMsg
mkCex t = DsCex (Text.splitOn "," t)

msgp :: ParsecT Text () IO EmuMsg
msgp = 
  string "DROIDSTAR:NG:" 
  >> choice 
       [ mkOk     <$> (string "QUERY:OK:" >> space >> getInput)
       , mkNo     <$> (string "Query:NO:" >> space >> getInput)
       , DsCheck  <$> (string "CHECK:" >> space >> getInput >>= liftIO . mkTS)
       , mkCex    <$> (string "CEX:" >> space >> getInput)
       , DsResult <$> (string "RESULT:" >> space >> getInput) ]
