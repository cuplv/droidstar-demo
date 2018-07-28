{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (FilePath)

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text
import Data.Text.Encoding (decodeUtf8,encodeUtf8)
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe
import Network.DNS.Resolver
import Network.DNS.Lookup
import Network.DNS.Types (DNSError)

import qualified System.IO as IO
import qualified System.Process as Proc

import Turtle
import qualified Control.Foldl as Foldl

import Data.Digest.Pure.SHA (sha1,showDigest)
import qualified Data.ByteString.Lazy as BL

data Config = Config { emuAddr :: Maybe String
                     , apksDir :: FilePath
                     , initDelay :: Int }

-- cfg = Config (Just "172.17.0.2") (fromText "..") 0
cfg = Config Nothing (fromText "/root/apks") 120

main :: IO ()
main = do 
  putStrLn "Starting..."
  IO.hFlush IO.stdout
  ip <- case emuAddr cfg of
          Just ip -> return $ Right [ip]
          Nothing -> getIP
  case ip of
    Right [ipaddr] -> 
      do Concurrent.threadDelay (1000 * 1000 * (initDelay cfg))
         connectAdb ipaddr
         state <- Concurrent.newMVar []
         Warp.run 30025 $ WS.websocketsOr
           WS.defaultConnectionOptions
           (wsApp state)
           httpApp
    _ -> die "No ip"

getIP :: IO (Either DNSError [String])
getIP = do 
  rs <- makeResolvSeed defaultResolvConf
  ip <- withResolver rs $ \resolver -> lookupA resolver "emulator.lan"
  return (fmap (map show) ip)

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

httpApp :: Wai.Application
httpApp request respond = do
  putStrLn "Handling an HTTP request..."
  case Wai.pathInfo request of
    ["res",img] -> do
      respond $ Wai.responseFile
        Http.status200
        [("Content-Type", "image/png")]
        ("./res/" ++ Text.unpack img)
        Nothing
    _ -> respond $ Wai.responseLBS
      Http.status400 
      []
      "Not a results request."

type ClientId = Int
type Client = (ClientId, WS.Connection)
type State = [Client]

nextId :: State -> ClientId
nextId = Maybe.maybe 0 ((+) 1) . Safe.maximumMay . List.map fst

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let clientId = nextId state
  WS.sendTextData conn ("Asdf2"::Text.Text)
  return ((clientId, conn) : state, clientId)
  
withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
  return $ withoutClient clientId state
  
listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
listen conn clientId stateRef = Monad.forever $ do
  msg <- WS.receiveData conn :: IO Text.Text
  let send = WS.sendTextData conn
  case msg of
    "req:AsyncTask" ->      experiment send "AsyncTask"
    "req:CountDownTimer" -> experiment send "CountDownTimer"
    _ -> putStrLn "Received unhandled request."

resultsPath :: Text
resultsPath = 
  "/sdcard/Android/data/edu.colorado.plv.droidStar.experiments/files/results"

experiment send name = do
  send "dbg:Running experiment..."
  shell "adb logcat -c" empty
  -- proc "adb" ["install",format fp apk] empty
  -- Turtle.stdout (inproc "adb" ["shell","am","start","-a","android.intent.action.MAIN","-n","edu.colorado.plv.droidstar.experiments/.MainActivity"] empty)
  launchExp name
  followLog send
  send "dbg:All done."
  sendResults send name
  shell "adb logcat -c" empty
  return ()

launchExp :: Text -> IO ()
launchExp name = 
  let a = "edu.colorado.plv.droidstar.experiments/." <> name <> "Activity"
  in proc "adb" ["shell","am","start","-n",a] empty >> return ()

sendResults send name = do 
  Turtle.shell ("adb pull " <> resultsPath <> " ./") empty
  let graphPath = fromText "results" <> fromText (name <> "-diagram.gv")
  putStrLn (Text.unpack (format fp graphPath))
  graph <- readTextFile graphPath
  let graphHash = Text.pack . showDigest . sha1 . BL.fromStrict . encodeUtf8 $ graph
  let imgPath = "res/" <> graphHash <> ".png"
  shell "mkdir -p res" empty
  proc "dot" ["-Tpng","-o" <> imgPath,format fp graphPath] empty
  send $ "res:" <> imgPath
  putStrLn (Text.unpack imgPath)

followLog :: (Text -> IO ()) -> IO ()
followLog send = logcatDS >>= r
  where r (n,k) = do l <- n
                     if and [not (Text.isInfixOf "Completed learning" l)]
                        then do if Text.isInfixOf ":Q:" l
                                   then do let l' = "dbg:" <> snd (Text.breakOn ":Q:" l)
                                           send l'
                                           print l'
                                   else return ()
                                r (n,k) 

                        else k

logcatDS :: IO (IO Text,IO ())
logcatDS = do (_,Just hout,_,ph) <- Proc.createProcess 
                                      (Proc.proc "adb" ["logcat"]){ Proc.std_out = Proc.CreatePipe}
              let next = do l <- Text.pack <$> IO.hGetLine hout
                            if or [Text.isInfixOf "DROIDSTAR" l
                                  ,Text.isInfixOf "STARLING" l]
                               then return l
                               else next
              let kill = Proc.terminateProcess ph >> IO.hClose hout
              return (next,kill)

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef)
    (disconnectClient clientId stateRef)
