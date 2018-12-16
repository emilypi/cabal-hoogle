{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Distribution.CabalHoogle.HoogleOpts
  ( -- data
    HoogleOpts(..)
    -- utils
  , appendServerArgs
  , appendGenerativeArgs
    -- optics
  , setup
  , rebuild
  , startServer
  , additionalArgs
  ) where


import Control.Lens (makeLenses, (%~))

import Data.Bool (bool)

data HoogleOpts = HoogleOpts
  { _setup          :: Bool
  , _rebuild        :: Bool
  , _startServer    :: Bool
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
  appendArgs (bool id ((<>) (["server", "--local", "--port", "8080"])) _startServer) hc

appendGenerativeArgs :: HoogleOpts -> HoogleOpts
appendGenerativeArgs =
  appendArgs ((<>) ["generate", "--local"])
