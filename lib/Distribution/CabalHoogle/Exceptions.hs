{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTSyntax #-}
module Distribution.CabalHoogle.Exceptions
 ( CHException(..)
 ) where


import System.Exit

import Control.Exception (Exception(..))
import Data.Typeable


data CHException where
  NotInstalled  :: String   -> CHException
  NoHoogleDb    :: String   -> CHException
  HoogleVersion :: String   -> CHException
  Unexpected    :: ExitCode -> CHException
  deriving (Show, Eq, Ord, Typeable)

instance Exception CHException
