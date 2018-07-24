{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe
import Network.DNS.Resolver
import Network.DNS.Lookup
import Network.DNS.Types (DNSError)

import Turtle
import qualified Control.Foldl as Foldl

main :: IO ()
main = do 
  Concurrent.threadDelay (1000 * 1000 * 120)
  putStrLn "Starting..."
  ip <- getIP
  case ip of
    Right [ipaddr] -> 
      do connectAdb ipaddr
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
  e <- proc "adb" ["connect",Text.pack ip] empty
  case e of
    ExitFailure _ -> do
      putStrLn "Emulator was not ready, retrying..."
      Concurrent.threadDelay (1000 * 1000 * 4)
      connectAdb ip
    ExitSuccess -> do
      putStrLn "Connected to emulator."

httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

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
  -- WS.receiveData conn >>= broadcast clientId stateRef
  msg <- WS.receiveData conn :: IO Text.Text
  let send = WS.sendTextData conn
  case msg of

    "req:AsyncTask" -> do 
      send "dbg:Setting up..."
      send "dbg:Asdf1"
      send "dbg:Asdf2"
      send "dbg:Asdf3"
      send "dbg:Asdf4"
      send "dbg:Asdf5"
      send "dbg:Asdf6"
      send "dbg:Asdf7"
      send "dbg:Asdf8"
      stdout (inproc "echo" ["Doing a thing..."] empty)
      file <- readFile "./a.txt"
      send . Text.pack $ ("rsl:" ++ file)

    "req:CountDownTimer" -> do
      send "dbg:Running experiment..."
      proc "adb" ["install","/root/droidstar-debug.apk"] empty
      stdout (inproc "adb" ["shell","am","start","-a","android.intent.action.MAIN","-n","edu.colorado.plv.droidstar.experiments/.MainActivity"] empty)
      foldIO (grep (choice [has "DROIDSTAR",has "STARLING"]) (inshell "adb logcat" empty)) (Foldl.mapM_ ((\a -> send a >> print a) . ("dbg:" <>) . lineToText))
    
wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef)
    (disconnectClient clientId stateRef)
