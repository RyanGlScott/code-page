{-|
Module:      Tests
Copyright:   (C) 2016-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: Portable

Ensures that functions from "System.IO.CodePage" work properly.
-}
module Main (main) where

import System.IO.CodePage

printUnicodeStrings :: IO ()
printUnicodeStrings = do
  putStrLn "κόσμε"
  putStrLn "→"
  putStrLn "☀☁☂☃☄"

main :: IO ()
main = withCP1252 $
       withCodePageOptions defaultOptions{chatty = True} cp65001 printUnicodeStrings
