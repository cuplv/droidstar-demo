{-# LANGUAGE OverloadedStrings #-}

module ClientComm (CMsg (..), sendCMsg) where

import Prelude hiding (FilePath)

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson
import qualified Network.WebSockets as WS

data CMsg = CAlert Text
          | CQueryOk [Text] [Text]
          | CQueryNo [Text]
          | CCheck Text
          | CResult Text

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
      CResult uri -> object ["result" .= object ["uri" .= toJSON uri]]

sendCMsg :: WS.Connection -> CMsg -> IO ()
sendCMsg conn = WS.sendTextData conn . encode