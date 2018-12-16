module Distribution.CabalHoogle.Internal
  ( versionCmd
  , haddockCmd
  , generateCmd) where

import System.Process (CreateProcess(..), createProcess, proc)
