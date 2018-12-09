{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTSyntax #-}
module Distribution.CabalHoogle.Exceptions
 ( CHException(..)
 ) where

import Control.Exception (Exception(..))
import Data.Text

data CHException where
  NotInstalled  :: Text -> CHException
  Unexpected    :: Text -> CHException
  CouldNotSetup :: Text -> CHException
  NoHoogleDb    :: Text -> CHException
  deriving (Show, Eq, Ord)

instance Exception CHException
