module Main where


import System.Environment (getArgs)
import Distribution.CabalHoogle (cabalHoogle)

main :: IO ()
main = getArgs >>= cabalHoogle
