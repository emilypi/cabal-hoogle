module Main where


import System.Exit (ExitCode(..))

import Options.Applicative

import Control.Exception (bracket_, catch)
-- internal modules

import Distribution.CabalHoogle.Exceptions
import Distribution.CabalHoogle.HoogleConfig (mkDefaultConfig)
import Distribution.CabalHoogle.HoogleCmd (hoogleCmd, prettyPrint)
import Distribution.CabalHoogle.HoogleOpts (optParser)

main :: IO ()
main = do
  hoogleOpts <- execParser opts
  env        <- mkDefaultConfig
  bracket_ startupGracefully shutdownGracefully $
    hoogleCmd hoogleOpts env `catch` handleError
  where
    opts = info (optParser <**> helper)
      (  fullDesc
      <> progDesc "Enable hoogle and haddock generation"
      <> header   "cabal-hoogle - an agnostic hoogle plugin for Cabal"
      )
    -- Handle errors in the dumbest possible way, as long as they are handled
    -- We only care about the error messages percolated up through the callstack
    handleError err = prettyPrint $ case err of
      NotInstalled e         -> "\x1F62D [cabal-hoogle] " <> e
      NoHoogleDb e           -> "\x1F62D [cabal-hoogle] " <> e
      HoogleVersion e        -> "\x1F62D [cabal-hoogle] " <> e
      Unexpected ExitSuccess -> "\x1F44F [cabal-hoogle] Successfully started"
      Unexpected _           -> "\x1F47A [cabal-hoogle] Unknown exception occurred"

    startupGracefully = prettyPrint "\x1F305 [cabal-hoogle] Starting up..."
    shutdownGracefully = prettyPrint "\x1F47E [cabal-hoogle] Shutting down..."
