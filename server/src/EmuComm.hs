{-# LANGUAGE OverloadedStrings #-}

module EmuComm 
  ( module EmuMsg
  , EmuReady
  , connectAdb
  , installAdb
  , uninstallAdb
  , mkReadyAdb
  , apkPath
  , droidstarPath
  , droidstarCustomPath
  , refreshCustom
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

-- import Config
import EmuMsg

data EmuReady = EmuReady

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

installAdb :: FilePath -> IO ()
installAdb apk = do
  proc "adb" ["install","-r",format fp apk] empty
  putStrLn "Installed experiment."
  IO.hFlush IO.stdout

uninstallAdb :: IO ()
uninstallAdb = do
  proc "adb" ["uninstall","edu.colorado.plv.droidstar.experiments"] empty
  clearLog
  putStrLn "Ensured any previous experiment is uninstalled/killed."
  IO.hFlush IO.stdout

mkReadyAdb :: IO EmuReady
mkReadyAdb = uninstallAdb >> return EmuReady

apkPath :: FilePath
apkPath = fromText "driver-app/target/android/output/droidstar-debug.apk"

droidstarPath = fromText "droidstar"

droidstarCustomPath = fromText "droidstar-custom"

refreshCustom :: IO ()
refreshCustom = do
  h <- home
  t <- testdir (h <> droidstarCustomPath)
  if t
     then rmtree (h <> droidstarCustomPath)
     else return ()
  cptree (h <> droidstarPath) (h <> droidstarCustomPath)

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
                               then do (mmsg) <- runParserT msgp () "" l
                                       case mmsg of
                                         Right msg -> return msg
                                         Left e -> die (Text.pack (show e))
                               else next
              let kill = Proc.terminateProcess ph >> IO.hClose hout
              return (next,kill)
