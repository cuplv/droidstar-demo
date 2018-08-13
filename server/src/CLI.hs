module CLI (Conf (..), getConf) where

import Options.Applicative
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import ClientComm (ServerMode (..))

data Conf = Conf 
  { confServerMode :: ServerMode
  , confHome :: Maybe Text
  , confAddr :: Maybe Text
  , confInitDelay :: Int }

confp :: Parser Conf
confp = Conf 
  <$> flag StaticMode CustomMode
        ( long "enable-custom"
       <> help "Enable custom experiments (allows arbitrary user-written \
               \Java code to run inside the Android emulator)")
  <*> mtext (long "home-dir" 
             <> metavar "PATH" 
             <> help "Set a non-standard home directory")
  <*> mtext (long "emu-address"
             <> metavar "ADDR"
             <> help "Set a specific emulator address")
  <*> option auto 
        ( long "init-delay"
       <> value 0
       <> metavar "SECONDS"
       <> help "Set a starting delay before connecting to \
               \the emulator")

mtext :: Mod OptionFields Text -> Parser (Maybe Text)
mtext ms = 
  (Just <$> strOption ms) <|> pure Nothing

opts :: ParserInfo Conf
opts = info (confp <**> helper)
  ( fullDesc
  <> progDesc "Run the DroidStar demo server, which will bridge the \
              \emulaor and web client")

getConf :: IO Conf
getConf = execParser opts
