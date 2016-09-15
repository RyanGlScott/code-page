{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module:      System.Win32.CodePage
Copyright:   (C) 2016 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: Portable

On Windows, this exports functions for getting, setting, and analyzing code pages.
On other operating systems, this exports nothing.
-}
module System.Win32.CodePage (
#ifdef WINDOWS
      -- * The 'CodePage' type
      CodePage

      -- * Getting and setting code pages
    , getConsoleCP
    , getConsoleOutputCP
    , setConsoleCP
    , setConsoleOutputCP
    , getACP
    , getOEMCP

    -- * Code page encodings
    , codePageEncoding
    , mkCodePageEncoding

    -- * Valid code pages
    , isValidCodePage
    , installedCodePages
    , supportedCodePages

    -- * 'CodePage'-aware Unicode conversion
    , stringToUnicode
#endif
    ) where

#ifdef WINDOWS
import Data.IORef
import Data.List

import Foreign.C.String
import Foreign.Ptr

import GHC.IO.Encoding.CodePage

import System.Win32.Console
import System.Win32.NLS
import System.Win32.Types

#  include <windows.h>
## include "windows_cconv.h"

type CODEPROC_ENUMW = LPWSTR -> IO BOOL

foreign import WINDOWS_CCONV "windows.h EnumSystemCodePagesW"
    c_EnumSystemCodePagesW :: FunPtr CODEPROC_ENUMW -> DWORD -> IO BOOL

str_EnumSystemCodePagesW :: String
str_EnumSystemCodePagesW = "EnumSystemCodePagesW"

foreign import WINDOWS_CCONV "wrapper"
    mkCodeProcEnumW :: CODEPROC_ENUMW -> IO (FunPtr CODEPROC_ENUMW)

cP_INSTALLED, cP_SUPPORTED :: DWORD
cP_INSTALLED = #{const CP_INSTALLED}
cP_SUPPORTED = #{const CP_SUPPORTED}

-- | Enumerates all installed code pages in sorted order.
installedCodePages :: IO [CodePage]
installedCodePages = systemCodePages cP_INSTALLED

-- | Enumerates all supported code pages in sorted order.
supportedCodePages :: IO [CodePage]
supportedCodePages = systemCodePages cP_SUPPORTED

systemCodePages :: DWORD -> IO [CodePage]
systemCodePages flags = do
    cpRef <- newIORef []
    fptr <- mkCodeProcEnumW $ \cpLPWStr -> do
        cpStr <- peekCWString cpLPWStr
        modifyIORef cpRef (read cpStr :)
        return True
    failIfFalse_ str_EnumSystemCodePagesW $ c_EnumSystemCodePagesW fptr flags
    freeHaskellFunPtr fptr
    modifyIORef cpRef sort
    readIORef cpRef
#endif
