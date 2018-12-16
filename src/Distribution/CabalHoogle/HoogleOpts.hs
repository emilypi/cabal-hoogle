{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.CabalHoogle.HoogleOpts
  ( -- data
    HoogleOpts(..)
    -- parser
  , optParser
    -- utils
  , appendServerArgs
  , appendGenerativeArgs
    -- optics
  , setup
  , rebuild
  , startServer
  , serverPort
  , additionalArgs
  ) where


import Control.Lens (makeLenses, (%~))

import Data.Bool (bool)

import Options.Applicative


data HoogleOpts = HoogleOpts
  { _setup          :: Bool
  , _rebuild        :: Bool
  , _startServer    :: Bool
  , _serverPort     :: String
  , _additionalArgs :: [String]
  }
makeLenses ''HoogleOpts


appendArgs
  :: ([String] -> [String])
  -> HoogleOpts
  -> HoogleOpts
appendArgs = (%~) additionalArgs

appendServerArgs :: HoogleOpts -> HoogleOpts
appendServerArgs hc@HoogleOpts{..} =
  appendArgs (bool id ((<>) (["server", "--local", "--port", _serverPort])) _startServer) hc

appendGenerativeArgs :: HoogleOpts -> HoogleOpts
appendGenerativeArgs =
  appendArgs ((<>) ["generate", "--local"])

optParser :: Parser HoogleOpts
optParser = HoogleOpts
  <$> switch
      (  long "setup"
      <> short 's'
      <> help "When the --setup flag is enabled, a hoogle db and haddocks will be generated. \
              \If --rebuild is not flagged and the hoogle db exists, nothing will be rebuilt.\
              \If the hoogle db does not exist, then --setup or --rebuild must be flagged."
      )
  <*> switch
      (  long "rebuild"
      <> short 'r'
      <> help "When the --rebuild flag is enabled, the hoogle db and haddocks will be generated.\
              \If --rebuild is not flagged and the hoogle db exists, nothing will be rebuilt."
      )
  <*> switch
      (  long "start-server"
      <> help "When the --start-server flag is enabled, a hoogle server will be spawned.\
              \The default port is 8080, and may be set using the --port flag."
      )
  <*> option auto
      (  long "port"
      <> short 'p'
      <> help "When a port is specified, it will be used as the hoogle server port if \
              \the --start-server flag is also enabled."
      <> showDefault
      <> value "8080"
      <> metavar "PORT"
      )
  <*> pure []
