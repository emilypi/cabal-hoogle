{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.CabalHoogle.HoogleCmd
  ( hoogleCmd
  ) where


import GHC.IO.Handle (hGetContents)

import System.Directory (findExecutable, doesFileExist, createDirectoryIfMissing)
import System.Exit (ExitCode(..), exitWith)
import System.Process (proc, readCreateProcess, createProcess)

import Control.Monad.Catch (throwM)

import Data.Bool (bool)
import Data.Functor (void)
import Data.Semigroup ((<>))
import Data.Text hiding (takeWhile)

import Distribution.CabalHoogle.Exceptions
import Distribution.CabalHoogle.HoogleConfig (HoogleConfig(..), minHoogleVersion)
import Distribution.CabalHoogle.HoogleOpts (HoogleOpts(..), appendServerArgs, appendGenerativeArgs)

import Distribution.Text (simpleParse)
import Distribution.Version (Version(..))


-- | Main commands ------------------------------------------------


-- | Check for 'hoogle' binary in given path
-- and run hoogle with built command set. All
-- errors should be logged to console.
hoogleCmd
  :: HoogleOpts
  -> HoogleConfig
  -> IO ()
hoogleCmd opts env = do
  hooglePath <- findHoogleExecutable env
  handleOpts hooglePath opts env


handleOpts
  :: FilePath
  -> HoogleOpts
  -> HoogleConfig
  -> IO ()
handleOpts hooglePath opts@HoogleOpts{..} env = do
  dbExists <- doesFileExist (_hoogleDatabasePath env)
  -- if db exists and rebuild is not set, do nothing else setup and rebuild
  bool rebuildOrSetup (pure ()) $ dbExists && (not _rebuild)
  -- if server is enabled, add local server opts
  runHoogle hooglePath env (appendServerArgs opts)
  where
    rebuildOrSetup =
      if _setup || _rebuild
      then generateDB hooglePath env opts >> generateHaddocks
      else cantSetup >> exitEarly

    cantSetup = throwM . NoHoogleDb $
      "No Hoogle database found. Please use the --setup or --rebuild\
      \flags to ensure that a database is build if not found."

-- | Run the 'hoogle' executable given a set of
-- arguments and configuration details. This will
-- have the following arguments:
--
--  * if '--setup' or '--rebuild' flags were enabled, or if an existing
--    database is not found, then haddocks and database will be generated
--
--  * if only '--setup' was set, then haddocks, and database
--    will be generated if the database does not exist or '--rebuild'
--    is flagged also (or both)
--
--  * if '--server' is also set, then a server will be started locally
--    on port 8080.
runHoogle
  :: FilePath
  -> HoogleConfig
  -> HoogleOpts
  -> IO ()
runHoogle hooglePath HoogleConfig{..} HoogleOpts{..} = do
  let dbArgs = ["--database=" <> _hoogleDatabasePath]
  void $ createProcess $ proc hooglePath (_additionalArgs <> dbArgs)

-- | Using the process config and 'System.Directory',
-- grep available $PATH$ information for hoogle
-- executables and return the absolute filepath
-- if 'hoogle' has an amenable version
findHoogleExecutable :: HoogleConfig -> IO FilePath
findHoogleExecutable env = do
  mHooglePath <- findExecutable "hoogle"
  maybe notInstalled (checkVersion env) mHooglePath
  where
    notInstalled =
      throwM . NotInstalled $ "Hoogle executable not found"


-- | In the case where the '--setup' flag is enabled,
-- this will trigger the construction of a db file with
-- which to point the 'hoogle' executable at. When no
-- '--setup' flag is enabled, and the db file is not
-- located in the relative path, this will error.
generateDB
  :: FilePath
  -> HoogleConfig
  -> HoogleOpts
  -> IO ()
generateDB hooglePath env@HoogleConfig{..} opts = do
  createDirectoryIfMissing True _hoogleProjectRoot
  runHoogle hooglePath env (appendGenerativeArgs opts)

-- | In the case where the '--generate' flag is enabled
-- this will trigger the generation of 'haddock' information
-- via the 'hoogle' executable and write to generated db file.
-- Note that if no '--setup' flag has been passed, the db file
-- must be present in order for this to succeed.
generateHaddocks :: IO ()
generateHaddocks = do
  -- TODO: this is utter stupidity
  cabal <- findCabalExecutable
  void $ createProcess $ proc cabal ["new-haddock"]
  where
    findCabalExecutable = do
      mCabalPath <- findExecutable "cabal"
      maybe notInstalled pure mCabalPath

    notInstalled =
      throwM . NotInstalled $ "Cabal executable not found"

-- | If '--setup' is enabled, then if no 'hoogle' executable
-- is located, 'cabal-hoogle' will attempt to install it via
-- 'cabal v2-install', and carry on with the setup.
installHoogle :: HoogleConfig -> IO ()
installHoogle HoogleConfig{..} = undefined

-- | Utilities --------------------------------------------------


-- | Exit early with prejudice
exitEarly :: IO a
exitEarly = exitWith . ExitFailure $ -1

-- | Check the version given a path to a valid
-- 'hoogle' executable
checkVersion
  :: HoogleConfig
  -> FilePath
  -> IO FilePath
checkVersion HoogleConfig{..} fp = do
  rawVersion <- versionProc fp
  version    <- parseVersion rawVersion
  bool (wrongVersion fp version) (pure fp) $
    version >= _minHoogleVersion
  where
    wrongVersion p v = throwM . HoogleVersion . pack
      $ "Hoogle executable located at '"
      <> p
      <> "' has incompatible version: "
      <> show v

    versionProc p =
      readCreateProcess (proc p ["--numeric-version"]) ""

    parseVersion s =
      case simpleParse s of
        Nothing ->
          throwM . HoogleVersion $ "Version string malformed"
        Just v  ->  pure v
