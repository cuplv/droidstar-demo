{-# LANGUAGE OverloadedStrings #-}

module ClientComm 
  ( CMsg (..)
  , sendCMsg
  , SReq (..)
  , ServerMode (..)
  , LangMode (..)
  , lmodetext
  ) where

import Prelude hiding (FilePath)

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson
import qualified Network.WebSockets as WS

data CMsg = CAlert Text
          | CQueryOk [Text] [Text]
          | CQueryNo [Text]
          | CCheck Text
          | CCex [Text]
          | CResult Text
          | CCompiled
          | CHello ServerMode
          | CCompileError Text

instance ToJSON CMsg where
  toJSON m = 
    case m of
      CAlert t -> object ["alert" .= object ["message" .= toJSON t]]
      CQueryOk is os -> 
        object ["queryOk" .= object ["inputs" .= toJSON is
                                    ,"outputs" .= toJSON os]]
      CQueryNo is ->
        object ["queryNo" .= object ["inputs" .= toJSON is]]
      CCheck uri -> object ["check" .= object ["uri" .= toJSON uri]]
      CCex is -> 
        object ["cex" .= object ["inputs" .= toJSON is]]
      CResult uri -> object ["result" .= object ["uri" .= toJSON uri]]
      CCompiled -> object ["compiled" .= toJSON ("compiled" :: Text)]
      CHello m -> object ["hello" .= toJSON (case m of
                                               StaticMode -> "static" :: Text
                                               CustomMode -> "custom" :: Text)]
      CCompileError t -> object ["compileError" .= toJSON t]

data ServerMode = StaticMode | CustomMode

data LangMode = ScalaMode | JavaMode

lmodetext :: LangMode -> Text
lmodetext ScalaMode = "scala"
lmodetext JavaMode = "java"

data SReq = SReq Text Text LangMode

instance FromJSON SReq where
  parseJSON = withObject "SReq" $ \v -> do
    n <- v .: "name"
    lp <- v .: "lp"
    lm <- v .: "lang"
    lmode <- if lm == "java"
                then return JavaMode
                else if lm == "scala"
                        then return ScalaMode
                        else fail $ "No language mode \"" ++ (Text.unpack lm) ++ "\""
    return (SReq n lp lmode)

sendCMsg :: WS.Connection -> CMsg -> IO ()
sendCMsg conn = WS.sendTextData conn . encode
