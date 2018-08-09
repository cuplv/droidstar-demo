{-# LANGUAGE OverloadedStrings #-}

module Sbt (genLpApk) where

import Prelude hiding (FilePath)

import Data.Text (Text)
import qualified Data.Text as Text
import Turtle
-- import qualified System.IO as IO
-- import qualified System.Process as Proc
-- import qualified Control.Concurrent as Concurrent

(<!>) :: (Monoid a, Applicative m) => m a -> m a -> m a
(<!>) a b = mappend <$> a <*> b

projDir = home <!> pure (fromText "droidstar")

sbtTarget = "driverApp/android:package"

lpDir = projDir <!> pure (fromText "driver-app/src/main/scala/edu/colorado/plv/droidstar/experiments/lp")

lpFile = lpDir <!> pure (fromText "CustomLP.scala")

writeLp :: Text -> IO ()
writeLp t = (\f -> writeTextFile f t) =<< lpFile

-- | Compile the project and return the path to the produced apk
compileLp :: IO (Either Text FilePath)
compileLp = do
  pd <- projDir
  cd pd
  (ec,err) <- procStrict "sbt" [sbtTarget] empty
  cd =<< home
  case ec of
    ExitSuccess -> return.Right$ (pd <> "driver-app/target/android/output/droidstar-debug.apk")
    ExitFailure _ -> return.Left$ err

genLpApk :: Text -> IO (Either Text FilePath)
genLpApk lp = writeLp lp >> compileLp
