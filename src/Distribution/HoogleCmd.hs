{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Distribution.HoogleCmd where


import System.Directory (findExecutable)
import System.Exit (ExitCode(..), exitWith)
import System.Logger (Logger(..))

import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString.Lazy as BS
import Data.Reflection
import qualified Data.Text as T (Text, takeWhile)

import Distribution.Exceptions

-- TODO
data ProcessConfig = ProcessConfig

-- | Check for 'hoogle' binary in given path
-- and run hoogle with built command set. All
-- errors should be logged to console.
runHoogle
  :: Given ProcessConfig
  => MonadIO m
  => MonadThrow m
  => FilePath
  -> [T.Text]
  -> m ()
runHoogle = undefined

-- | Using the process config and 'System.Directory',
-- grep available $PATH$ information for hoogle
-- executables and return the absolute filepath
-- if 'hoogle' has an amenable version
findHoogleExecutable
  :: Given ProcessConfig
  => MonadIO m
  => MonadThrow m
  => m FilePath
findHoogleExecutable = do
  mHooglePath <- liftIO $ findExecutable "hoogle"
  maybe notInstalled checkVersion mHooglePath
  where
    notInstalled =
      throwM . NotInstalled $ "Hoogle executable not found"

    wrongVersion p v =  throwM . HoogleVersion
      $  "Hoogle executable located at '"
      <> p
      <> "' has incompatible verison: "
      <> v

    -- TODO: Vendor typed-processe
    versionCmd env p = proc p ["--numeric-version"] $
      tryAny . fmap fst $ readProcess_ env

    checkVersion hooglePath = liftIO $ do
      procEnv <- give
      version <- versionCmd procEnv p
      either (wrongVersion hooglePath) (extractVersion procEnv hooglePath) $
        version

    extractVersion env hooglePath bs =
      let versionBS = BS.unpack bs
      in case parseVersion (takeWhile (not . isSpace) versionBS) of
        Nothing -> wrongVersion hooglePath versionBS
        Just v  ->
          bool (wrongVersion hooglePath) (pure hooglePath) $
            v >= (env ^. minHoogleVersion)


-- | In the case where the '--setup' flag is enabled,
-- this will trigger the construction of a db file with
-- which to point the 'hoogle' executable at. When no
-- '--setup' flag is enabled, and the db file is not
-- located in the relative path, this will error.
generateDB
  :: Given ProcessConfig
  => MonadIO m
  => MonadThrow m
  => m ()
generateDB = undefined

-- | In the case where the '--generate' flag is enabled
-- this will trigger the generation of 'haddock' information
-- via the 'hoogle' executable and write to generated db file.
-- Note that if no '--setup' flag has been passed, the db file
-- must be present in order for this to succeed.
generateHaddocks
  :: Given ProcessConfig
  => MonadIO m
  => MonadThrow m
  => m ()
generateHaddocks = undefined

-- | If '--setup' is enabled, then if no 'hoogle' executable
-- is located, 'cabal-hoogle' will attempt to install it via
-- 'cabal v2-install', and carry on with the setup.
installHoogle
  :: Given ProcessConfig
  => MonadIO m
  => MonadThrow m
  => m ()
installHoogle = undefined

exitEarly
  :: forall a m
   . MonadIO m
  => MonadThrow m
  => m a
exitEarly =
  liftIO . exitWith . ExitFailure $ -1
