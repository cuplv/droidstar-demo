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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL

import qualified Text.Parsec as Ps

import Data.Aeson (eitherDecode,FromJSON)

import EmuComm
import ClientComm
import Sbt
import CLI

enabledLPs = 
  ["AsyncTask"
  ,"CountDownTimer"
  ,"SQLiteOpenHelper"
  ,"FileObserver"
  ,"VelocityTracker"]

main :: IO ()
main = do 
  (Conf mode mhome maddr mdelay) <- getConf
  case mhome of
    Just h -> export "HOME" h
    Nothing -> return ()
  ip <- case maddr of
          Just ip -> return $ Right [Text.unpack ip]
          Nothing -> getIP
  case mode of
    StaticMode -> putStrLn "Running droidstar-demo-server \
                           \in static mode."
    CustomMode -> putStrLn "Running droidstar-demo-server \
                           \with custom experiments enabled."
  case ip of
    Right [ipaddr] -> 
      do Concurrent.threadDelay (1000 * 1000 * mdelay)
         connectAdb ipaddr
         state <- Concurrent.newMVar []
         Warp.run 30025 $ WS.websocketsOr
           WS.defaultConnectionOptions
           (wsApp mode state)
           httpApp
    _ -> die "No ip"

getIP :: IO (Either DNSError [String])
getIP = do 
  rs <- makeResolvSeed defaultResolvConf
  ip <- withResolver rs $ \resolver -> lookupA resolver "emulator.lan"
  return (fmap (map show) ip)

httpApp :: Wai.Application
httpApp request respond = do
  putStrLn "Handling an HTTP request..."
  case Wai.pathInfo request of
    ["typestates",img] -> do
      respond $ Wai.responseFile
        Http.status200
        [("Content-Type", "image/png")]
        ("./typestates/" ++ Text.unpack img)
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

connectClient :: ServerMode -> WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient mode conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let clientId = nextId state
  sendCMsg conn (CHello mode)
  return ((clientId, conn) : state, clientId)
  
withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state -> do
  uninstallAdb
  putStrLn "Disconnected and cleaned up client session."
  IO.hFlush IO.stdout
  return $ withoutClient clientId state
  
receiveJSON :: (FromJSON a) => WS.Connection -> IO a
receiveJSON conn = do 
  r <- eitherDecode <$> WS.receiveData conn
  case r of
    Right a -> return a
    Left s -> die (Text.pack s)

listen :: ServerMode -> WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
listen mode conn clientId stateRef = Monad.forever $ do
  (SReq name lp l) <- receiveJSON conn
  let send = sendCMsg conn
  case mode of
    StaticMode -> if or (map (== name) enabledLPs)
                     then experiment send (SReq name lp l)
                     else die $ "Class " <> name <> " not supported."
    CustomMode -> experimentCustom send (SReq name lp l) >> uninstallAdb

dbgMsg send t = do
  send (CAlert t)
  putStrLn (Text.unpack t)
  IO.hFlush IO.stdout

experiment send (SReq name _ _) = do
  h <- home
  installAdb (h <> fromText "droidstar" <> apkPath)
  runE name send

experimentCustom send (SReq name lp l) = do
  refreshCustom
  res <- genLpApk name lp l
  case res of
    Right f -> do
      installAdb f
      send CCompiled
      runE name send
    Left e -> do
      send $ CCompileError e

runE name send = do
  clearLog
  launchExp name
  followLog send
  dbgMsg send "All done."

followLog :: (CMsg -> IO ()) -> IO ()
followLog send = logcatDS >>= r
  where r (next,kill) = do
          msg <- next
          print msg
          let more = r (next,kill)
          case msg of
            DsQueryOk is os -> send (CQueryOk is os) >> more
            DsQueryNo is -> send (CQueryNo is) >> more
            DsCex is -> send (CCex is) >> more
            DsCheck t -> send (CCheck t) >> more
            DsResult t -> send (CResult t) >> kill

wsApp :: ServerMode -> Concurrent.MVar State -> WS.ServerApp
wsApp mode stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient mode conn stateRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen mode conn clientId stateRef)
    (disconnectClient clientId stateRef)
