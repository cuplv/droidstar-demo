{-# LANGUAGE OverloadedStrings #-}

module EmuComm 
  ( module EmuMsg
  , connectAdb
  , logcatDS
  , clearLog
  , launchExp
  ) where

import Prelude hiding (FilePath)

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec (runParserT)
import Turtle
import qualified System.IO as IO
import qualified System.Process as Proc
import qualified Control.Concurrent as Concurrent

import Config
import EmuMsg

connectAdb :: String -> IO ()
connectAdb ip = do
  (_,o) <- procStrict "adb" ["connect",Text.pack ip] empty
  if Text.isInfixOf "unable" o
     then do putStrLn "Emulator was not ready, retrying in 10s..."
             IO.hFlush IO.stdout
             Concurrent.threadDelay (1000 * 1000 * 10)
             connectAdb ip
     else do putStrLn "Connected to emulator."
             IO.hFlush IO.stdout
             let apk = (apksDir cfg) <> "droidstar.apk"
             proc "adb" ["install","-r",format fp apk] empty
             putStrLn "Installed experiments."
             IO.hFlush IO.stdout

clearLog :: IO ()
clearLog = shell "adb logcat -c" empty >> return ()

launchExp :: Text -> IO ()
launchExp name = 
  let a = "edu.colorado.plv.droidstar.experiments/." <> name <> "Activity"
  in proc "adb" ["shell","am","start","-n",a] empty >> return ()

logcatDS :: IO (IO EmuMsg,IO ())
logcatDS = do (_,Just hout,_,ph) <- Proc.createProcess 
                                      (Proc.proc "adb" ["logcat"]){ Proc.std_out = Proc.CreatePipe}
              let next = do l <- Text.pack <$> IO.hGetLine hout
                            if Text.isInfixOf "DROIDSTAR:NG:" l
                               then do (Right msg) <- runParserT msgp () "" l
                                       return msg
                               else next
              let kill = Proc.terminateProcess ph >> IO.hClose hout
              return (next,kill)
