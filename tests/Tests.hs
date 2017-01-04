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

printUnicodeString :: IO ()
printUnicodeString = do
    putStrLn "κόσμε"
    putStrLn "→"
    putStrLn "☀☁☂☃☄"

main :: IO ()
main = withCodePageVerbosity True 65001 printUnicodeString
