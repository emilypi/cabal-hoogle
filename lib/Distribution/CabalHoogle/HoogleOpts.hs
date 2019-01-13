{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.CabalHoogle.HoogleOpts
  ( -- data
    HoogleOpts(..)
    -- parser
  , optParser
    -- utils
  , appendCommand
  , appendServerCommand
  , appendGenerativeCommand
    -- optics
  , setup
  , rebuild
  , startServer
  , noHaddocks
  , serverPort
  , hoogleCommand
  ) where


import Control.Lens (makeLenses, (%~))

import Data.Bool (bool)

import Options.Applicative


data HoogleOpts = HoogleOpts
  { _setup          :: Bool
  , _rebuild        :: Bool
  , _startServer    :: Bool
  , _noHaddocks     :: Bool
  , _serverPort     :: Int
  , _hoogleCommand  :: [String]
  }
makeLenses ''HoogleOpts


appendCommand
  :: ([String] -> [String])
  -> HoogleOpts
  -> HoogleOpts
appendCommand = (%~) hoogleCommand

appendServerCommand :: HoogleOpts -> HoogleOpts
appendServerCommand hc@HoogleOpts{..} =
  appendCommand (bool id ((<>) (["server", "--local", "--port", show _serverPort])) _startServer) hc

appendGenerativeCommand :: HoogleOpts -> HoogleOpts
appendGenerativeCommand =
  appendCommand ((<>) ["generate", "--local"])

optParser :: Parser HoogleOpts
optParser = HoogleOpts
  <$> switch
      (  long "setup"
      <> help "A hoogle db and haddocks will be generated. \
              \If --rebuild is not flagged and the hoogle db exists, nothing will be rebuilt.\
              \If the hoogle db does not exist, then --setup or --rebuild must be flagged."
      )
  <*> switch
      (  long "rebuild"
      <> help "When the '--rebuild' flag is enabled, the hoogle db and haddocks will be generated. \
              \If --rebuild is not flagged and the hoogle db exists, nothing will be rebuilt."
      )
  <*> switch
      (  long "start-server"
      <> help "When the '--start-server' flag is enabled, a hoogle server will be spawned. \
              \The default port is 8080, and may be set using the --port flag."
      )
  <*> switch
      ( long "no-haddocks"
      <> help "No haddocks will be generated."
      )
  <*> option auto
      (  long "port"
      <> help "When a port is specified, it will be used as the hoogle server port if \
              \the '--start-server' flag is also enabled."
      <> showDefault
      <> value 8080
      <> metavar "PORT"
      )
  <*> pure []
