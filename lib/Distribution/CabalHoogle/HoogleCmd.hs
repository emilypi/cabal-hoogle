{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Distribution.CabalHoogle.HoogleCmd
  ( hoogleCmd
  , prettyPrint
  ) where


import GHC.IO.Handle (hGetContents)

import System.Console.ANSI

import System.Directory (findExecutable, doesFileExist, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.Process (StdStream(..), proc, createProcess, waitForProcess,
                       readCreateProcess, std_out, delegate_ctlc)

import Control.Lens ((.~), (&))
import Control.Monad (when)
import Control.Monad.Catch (throwM)

import Data.Bool (bool)
import Data.Functor (void)
import Data.Semigroup ((<>))

import Distribution.CabalHoogle.Exceptions
import Distribution.CabalHoogle.HoogleConfig (HoogleConfig(..))
import Distribution.CabalHoogle.HoogleOpts (HoogleOpts(..), appendGenerativeCommand,
                                            appendCommand, hoogleCommand)

import Distribution.Text (simpleParse)


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
  bool cantSetup (rebuildOrSetup dbExists) $ _rebuild || _setup
  -- if server is enabled, add local server opts
  when _startServer $ generateServer hooglePath opts
  where
    rebuildOrSetup dbExists = do
      -- when rebuild or setup are flagged and haddocks are okay, generate them
      bool skipHaddocks generateHaddocks $ (_rebuild || _setup) && not _noHaddocks
      if dbExists
        then
          bool skipRegen (generateDB hooglePath env opts) $ _setup || _rebuild
        else bool cantSetup (generateDB hooglePath env opts) $ _setup

    skipRegen = prettyPrint
      "\x1F64C [cabal-hoogle] Database file found. Because '--rebuild' \
      \was not set, and '--setup' does not rebuild, skipping \
      \db regeneration."

    skipHaddocks = prettyPrint
      "\x1F41F [cabal-hoogle] Haddocks will not be generated because \
      \'--no-haddocks' has been set"

    cantSetup = throwM . NoHoogleDb $
      "No Hoogle database found. Please use the --setup flag to build one."


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
  -> HoogleOpts
  -> IO ()
runHoogle hooglePath HoogleOpts{..} =
  squashStreamingProcess hooglePath _hoogleCommand

-- | Using the process config and 'System.Directory',
-- grep available $PATH$ information for hoogle
-- executables and return the absolute filepath
-- if 'hoogle' has an amenable version
findHoogleExecutable :: HoogleConfig -> IO FilePath
findHoogleExecutable env = do
  hooglePath <- findExecutable' "hoogle" notInstalled
  checkVersion env hooglePath
  where
    notInstalled =
      NotInstalled $ "Hoogle executable not found"

-- | Generate a hoogle server instance of the following format:
--
--  * hoogle server --local --port <userdefined|8080>
--
-- To enable this workflow, make sure to specify the flag '--start-server'
-- as a flag to 'cabal-hoogle'
generateServer
  :: FilePath
  -> HoogleOpts
  -> IO ()
generateServer hooglePath opts@HoogleOpts{..} = do
  prettyPrint "\x1F47B [cabal-hoogle] Starting Hoogle server..."
  let sOpts = opts & hoogleCommand .~ ["server", "--local", "--port", show _serverPort]
  runHoogle hooglePath sOpts

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
generateDB hooglePath HoogleConfig{..} opts = do
  prettyPrint "\x1F49B [cabal-hoogle] Generating Hoogle database..."
  createDirectoryIfMissing True _hoogleProjectRoot
  let dbArgs = ["--database=" <> _hoogleDatabasePath]
  runHoogle hooglePath $ appendGenerativeCommand . appendCommand ((<>) dbArgs) $ opts

-- | In the case where the '--generate' flag is enabled
-- this will trigger the generation of 'haddock' information
-- via the 'hoogle' executable and write to generated db file.
-- Note that if no '--setup' flag has been passed, the db file
-- must be present in order for this to succeed.
generateHaddocks :: IO ()
generateHaddocks = do
  prettyPrint "\x1F916 [cabal-hoogle] Generating Haddocks..."
  cabal <- findExecutable' "cabal" notInstalled
  squashStreamingProcess cabal ["v2-haddock", "--verbose=0"]
  where
    notInstalled =
      NotInstalled $ "Cabal executable not found in $PATH"

-- | If '--setup' is enabled, then if no 'hoogle' executable
-- is located, 'cabal-hoogle' will attempt to install it via
-- 'cabal v2-install', and carry on with the setup.
_installHoogle :: IO ()
_installHoogle = do
  cabal <- findExecutable' "cabal" notInstalled
  squashStreamingProcess cabal ["v2-install", "--verbose=0", "hoogle"]
  where
    notInstalled =
      NotInstalled $ "Cabal executable not found in $PATH"

-- | Utilities --------------------------------------------------

findExecutable'
  :: String
  -> CHException
  -> IO FilePath
findExecutable' cmd err = do
  mPath <- findExecutable cmd
  maybe (throwM err) pure mPath


squashStreamingProcess
  :: String
  -> [String]
  -> IO ()
squashStreamingProcess cmd args = do
  -- build process taking away SIGINT control
  (_, Just hOut, _, p) <- createProcess (proc cmd args)
    { std_out = CreatePipe
    , delegate_ctlc = False
    }
  -- dump std_out to void
  void $ hGetContents hOut
  exitCode  <- waitForProcess p
  handleUnexpected exitCode
  where
    handleUnexpected = \case
      ExitSuccess -> pure ()
      t           -> throwM . Unexpected $ t

-- | Check the version given a path to a valid
-- 'hoogle' executable
checkVersion
  :: HoogleConfig
  -> FilePath
  -> IO FilePath
checkVersion HoogleConfig{..} hooglePath = do
  rawVersion <- hoogleVersion
  version    <- parseVersion rawVersion
  bool (wrongVersion version) (pure hooglePath) $
    version >= _minHoogleVersion
  where
    wrongVersion v = throwM . HoogleVersion
      $ "Hoogle executable located at '"
      <> hooglePath
      <> "' has incompatible version: "
      <> show v

    hoogleVersion =
      readCreateProcess (proc hooglePath ["--numeric-version"]) ""

    parseVersion s =
      case simpleParse s of
        Nothing ->
          throwM . HoogleVersion $ "Version string malformed"
        Just v  ->  pure v

prettyPrint :: String -> IO ()
prettyPrint s = setSGR [SetColor Foreground Vivid Green]
  >> putStrLn s
  >> setSGR [Reset]
