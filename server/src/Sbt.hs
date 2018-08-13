{-# LANGUAGE OverloadedStrings #-}

module Sbt (genLpApk) where

import Prelude hiding (FilePath)

import Data.Text (Text)
import qualified Data.Text as Text
import Turtle

import EmuComm

(<!>) :: (Monoid a, Applicative m) => m a -> m a -> m a
(<!>) a b = mappend <$> a <*> b

projDir = home <!> pure droidstarCustomPath

sbtTarget = "driverApp/android:package"

lpDir = projDir <!> pure (fromText "driver-app/src/main/scala/edu/colorado/plv/droidstar/experiments/lp")

lpFile name = lpDir <!> pure (fromText $ name <> "LP.scala")

writeLp :: Text -> Text -> IO ()
writeLp name lp = (\f -> writeTextFile f lp) =<< lpFile name

-- | Compile the project and return the path to the produced apk
compileLp :: IO (Either Text FilePath)
compileLp = do
  pd <- projDir
  cd pd
  (ec,err) <- procStrict "sbt" ["-no-colors", sbtTarget] empty
  cd =<< home
  case ec of
    ExitSuccess -> return.Right$ (pd <> "driver-app/target/android/output/droidstar-debug.apk")
    ExitFailure _ -> return.Left$ err

genLpApk :: Text -> Text -> IO (Either Text FilePath)
genLpApk name lp = writeLp name lp >> compileLp
