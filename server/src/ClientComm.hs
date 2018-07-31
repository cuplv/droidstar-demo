module ClientComm where

data CMsg = CQueryOk [Text] [Text]
          | CQueryNo [Text] [Text]
          | CCheck Text
          | CResult Text
