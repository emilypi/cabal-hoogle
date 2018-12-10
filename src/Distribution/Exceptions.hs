{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTSyntax #-}
module Distribution.Exceptions
 ( CHException(..)
 ) where

import Control.Exception (Exception(..))
import Data.Text
import Data.Typeable


data CHException where
  NotInstalled  :: Text -> CHException
  Unexpected    :: Text -> CHException
  CouldNotSetup :: Text -> CHException
  NoHoogleDb    :: Text -> CHException
  HoogleVersion :: Text -> CHException
  deriving (Show, Eq, Ord, Typeable)

instance Exception CHException
