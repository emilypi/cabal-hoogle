module Main where


import System.Environment (getArgs)
import CabalHoogle (cabalHoogle)

main :: IO ()
main = getArgs >>= cabalHoogle
