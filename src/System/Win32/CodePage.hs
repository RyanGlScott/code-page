{-# LANGUAGE CPP #-}

-- TODO Docs
module System.Win32.CodePage (
#ifdef WINDOWS
      CodePage
    , getConsoleCP
    , getConsoleOutputCP
    , setConsoleCP
    , setConsoleOutputCP
    , getACP
    , getOEMCP
    , codePageEncoding
    , mkCodePageEncoding
#endif
    ) where

#ifdef WINDOWS
import GHC.IO.Encoding.CodePage

import System.Win32.Console
import System.Win32.NLS
#endif
