{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Typed
  ( -- data

    ProcessConfig
  , StreamSpec
  , StreamType(..)

    -- smartcons
  , proc
  , shell

    -- Convenient
  , setStdIn
  , setStdOut
  , setStdErr
  , setWorkingDir
  , setEnv
  , setEnvInherit
  , setCloseFds

    -- StreamSpec
  , mkStreamSpec
  , inherit
  , closed
  , byteStringInput
  , byteStringOutput
  , createPipe
  , useHandleOpen
  , useHandleClose

    -- Process Launch
  , startProcess
  , stopProcess
  , withProcess
  , readProcess

    -- ProcessConfig lenses
  , pcCmdSpec
  , pcStdIn
  , pcStdOut
  , pcStdErr
  , pcWorkingDir
  , pcEnv
  , pcCloseFds

    -- Process lenses
  , pConfig
  , pCleanup
  , pStdIn
  , pStdOut
  , pStdErr
  , pHandle
  , pExitCode

    -- StreamSpec lenses
  , ssStream
  , ssCreate
  ) where

-- Typed process needs some love here. Would like to do
-- this without importing another library

import System.Exit (ExitCode(..))
import System.IO   (Handle(..))
import qualified System.Process as P

import Control.Concurrent.STM.TMVar (TMVar(..))
import Control.Exception (bracket)
import Control.Lens
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString.Lazy
import Data.Reflection (Given(..), give)


----------------------------------------------------------------------
-- Data


data ProcessConfig i o e = ProcessConfig
  { _pcCmdSpec    :: P.CmdSpec
  , _pcStdIn      :: StreamSpec 'I i
  , _pcStdOut     :: StreamSpec 'O o
  , _pcStdErr     :: StreamSpec 'O e
  , _pcWorkingDir :: Maybe FilePath
  , _pcEnv        :: Maybe [(String, String)]
  , _pcCloseFds  :: Bool
  }
makeLenses ''ProcessConfig


defaultConfig :: ProcessConfig
defaultConfig = ProcessConfig
  { _pcCmdSpec    = ShellCommand ""
  , _pcWorkingDir = Nothing
  , _pcEnv        = Nothing
  , _pcCloseFds   = False
  }

data Process i o e = Process
  { _pConfig   :: ProcessConfig i o e
  , _pCleanup  :: IO ()
  , _pStdin    :: i
  , _pStdout   :: o
  , _pStderr   :: e
  , _pHandle   :: P.ProcessHandle
  , _pExitCode :: TMVar ExitCode
  }
makeLenses ''Process


newtype Cleanup a = Cleanup { runCleanup :: IO (a, IO ()) }
    deriving Functor

data StreamType = I | O

data StreamSpec (t :: StreamType) a = StreamSpec
  { _ssStream :: P.StdStream
  , _ssCreate :: ProcessConfig () () () -> Maybe Handle -> Cleanup a
  }
  deriving Functor
makeLenses ''StreamSpec

----------------------------------------------------------------------
-- Utilities

proc
  :: FilePath
  -> [String]
  -> ProcessConfig
proc = setProc defaultConfig


setProc
  :: ProcessConfig
  -> FilePath
  -> [String]
  -> ProcessConfig
setProc p cmd args =
  pcCmdSpec .~ (RawCommand cmd args) $ p


shell :: String -> ProcessConfig
shell cmd = setShell cmd defaultConfig


setShell
  :: String
  -> ProcessConfig
  -> ProcessConfig
setShell cmd =
  pcCmdSpec .~ ShellCommand cmd

setWorkingDir
  :: FilePath
  -> ProcessConfig
  -> ProcessConfig
setWorkingDir dir =
  pcWorkingDir .~ Just dir

setWorkingDirInherit
  :: ProcessConfig
  -> ProcessConfig
setWorkingDirInherit =
  pcWorkingDir .~ Nothing

setEnv
  :: [(String, String)]
  -> ProcessConfig
  -> ProcessConfig
setEnv env = pcEnv .~ Just env

setEnvInherit
  :: ProcessConfig
  -> ProcessConfig
setEnvInherit = pcEnv .~ Nothing

setCloseFds
    :: Bool
    -> ProcessConfig
    -> ProcessConfig
setCloseFds b = pcCloseFds .~ b


----------------------------------------------------------------------
-- Process Handlers

-- B I G   B O I alert
startProcess
  :: Given ProcessConfig
  => MonadIO m
  => m (Process i o e)
startProcess = undefined

stopProcess
  :: MonadIO m
  => Process i o e
  => m ()
stopProcess = liftIO . _pCleanup $ give

withProcess
  :: Given ProcessConfig
  => (ProcessConfig -> IO a)
  -> IO a
withProcess = bracket (startProcess give) stopProcess

readProcess
  :: Given ProcessConfig
  => MonadIO m
  => m (ExitCode, ByteString, ByteString)
readProcess =
  liftIO $ withProcess give result
  where
    result = undefined
