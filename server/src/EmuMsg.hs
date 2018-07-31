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
  deriving (Show,Eq,Ord)

mkOk :: Text -> EmuMsg
mkOk t = 
  case Text.splitOn ";" t of
    is:os:[] -> DsQueryOk (splitOn' "," is) (filter (\t -> t /= "+") (splitOn' "," os))

splitOn' sep t = case Text.splitOn sep t of
                   [""] -> []
                   r -> map rep r

rep :: Text -> Text
rep t | t == "delta" = "(CB?)"
      | t == "beta" = "(none)"
      | otherwise = t

mkNo :: Text -> EmuMsg
mkNo t = DsQueryNo (map rep (Text.splitOn "," t))

mkCex :: Text -> EmuMsg
mkCex t = DsCex (map rep (Text.splitOn "," t))

msgp :: ParsecT Text () IO EmuMsg
msgp = 
  manyTill anyToken (try (string "DROIDSTAR:NG:"))
  >> choice 
       [ mkOk     <$> try (string "QUERY:OK:" >> space >> getInput)
       , mkNo     <$> try (string "QUERY:NO:" >> space >> getInput)
       , DsCheck  <$> try (string "CHECK:" >> space >> getInput >>= liftIO . mkTS)
       , mkCex    <$> try (string "CEX:" >> space >> getInput)
       , DsResult <$> try (string "RESULT:" >> space >> getInput >>= liftIO . mkTS) ]
