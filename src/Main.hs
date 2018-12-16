{-# LANGUAGE LambdaCase #-}
module Main where


import System.Environment (getArgs)

import Options.Applicative

import Distribution.CabalHoogle.HoogleConfig (mkDefaultConfig)
import Distribution.CabalHoogle.HoogleCmd (hoogleCmd)
import Distribution.CabalHoogle.HoogleOpts (optParser)


main :: IO ()
main = do
  hoogleOpts <- execParser opts
  env        <- mkDefaultConfig
  hoogleCmd hoogleOpts env
  where
    opts = info (optParser <**> helper)
      (  fullDesc
      <> progDesc "Enable hoogle and haddock generation"
      <> header   "cabal-hoogle - an agnostic hoogle plugin for Cabal"
      )
