{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|
Module:      System.IO.CodePage
Copyright:   (C) 2016-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: Portable

Exports functions which adjust code pages on Windows, and do nothing on other
operating systems.
-}
module System.IO.CodePage (
      -- * Adjusting 'CodePage's
      withCP65001
    , withCP1200
    , withCP1201
    , withCP12000
    , withCP12001
    , withCP1252
    , withCodePage
    , withCodePageOptions

      -- * Notable 'CodePage's
    , CodePage
    , cp65001
    , cp1200
    , cp1201
    , cp12000
    , cp12001
    , cp1252

      -- * 'Options'
    , Options
    , defaultOptions
      -- ** Record fields of 'Options'
    , chatty
    , nonWindowsBehavior

      -- ** 'NonWindowsBehavior'
    , NonWindowsBehavior
      -- ** Constructing 'NonWindowsBehavior'
    , nonWindowsDoNothing
    , nonWindowsFallbackCodePageEncoding
    , defaultFallbackCodePageEncoding
    ) where

import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Foldable (forM_)
import System.IO ( TextEncoding, hGetEncoding, hPutStrLn, hSetEncoding
                 , stderr, stdin, stdout )
import System.IO.CodePage.Internal

#ifdef WINDOWS
import System.Win32.CodePage hiding (CodePage)
#else
import GHC.IO.Encoding (textEncodingName)
#endif

-- | Sets the code page for an action to UTF-8 as necessary.
withCP65001 :: IO a -> IO a
withCP65001 = withCodePage cp65001

-- | Sets the code page for an action to UTF-16LE as necessary.
withCP1200 :: IO a -> IO a
withCP1200 = withCodePage cp1200

-- | Sets the code page for an action to UTF-16BE as necessary.
withCP1201 :: IO a -> IO a
withCP1201 = withCodePage cp1201

-- | Sets the code page for an action to UTF-32LE as necessary.
withCP12000 :: IO a -> IO a
withCP12000 = withCodePage cp12000

-- | Sets the code page for an action to UTF-32BE as necessary.
withCP12001 :: IO a -> IO a
withCP12001 = withCodePage cp12001

-- | Sets the code page for an action to Latin1 as necessary.
withCP1252 :: IO a -> IO a
withCP1252 = withCodePage cp1252

-- | Sets the code page for an action as necessary.
--
-- On operating systems besides Windows, this will make an effort to change
-- the current 'TextEncoding' to something that is equivalent to the supplied
-- 'CodePage'. Currently, the only supported 'CodePage's on non-Windows OSes
-- are 'cp65001', 'cp1200', 'cp1201', 'cp12000', and 'cp12001'. Supplying any
-- other 'CodePage' will result in a runtime error on non-Windows OSes. (If you
-- would like to configure this behavior, use 'withCodePageOptions' instead.)
withCodePage :: CodePage -> IO a -> IO a
withCodePage = withCodePageOptions defaultOptions

-- | Sets the code page for an action as necessary. If the 'Bool' argument is 'True',
-- this function will emit a warning to @stderr@ indicating that the code page has
-- been changed. ('withCodePage' sets this argument to 'False'.)

-- Taken from the stack codebase
-- (https://github.com/commercialhaskell/stack/blob/21e517ba88b3c6bee475fb00ad95f280e7285a54/src/main/Main.hs#L82-L123)
-- which is under a 3-clause BSD license
withCodePageOptions :: Options -> CodePage -> IO a -> IO a
withCodePageOptions (Options{chatty, nonWindowsBehavior}) cp inner =
  case nonWindowsBehavior of
    NonWindowsDoNothing -> inner
    NonWindowsFallbackCodePageEncoding fallback -> do
#ifdef WINDOWS
      origCPI <- getConsoleCP
      origCPO <- getConsoleOutputCP
#else
      -- These are never used on non-Windows OSes,
      -- so their values are irrelevant
      let origCPI = 0
          origCPO = 0
#endif
      mbOrigStdinEnc  <- hGetEncoding stdin
      mbOrigStdoutEnc <- hGetEncoding stdout
      mbOrigStderrEnc <- hGetEncoding stderr

      let expected = codePageEncoding' fallback cp
#ifdef WINDOWS
          setInput  = origCPI /= cp
          setOutput = origCPO /= cp
#else
          -- Crude, but the best available option on non-Windows OSes
          setInput  = fmap textEncodingName mbOrigStdinEnc
                        /= Just (textEncodingName expected)
          setOutput = fmap textEncodingName mbOrigStdoutEnc
                        /= Just (textEncodingName expected)
#endif
          fixInput
              | setInput = bracket_
                  (do
                      setConsoleCP' cp
                      hSetEncoding stdin expected
                      )
                  (do
                      setConsoleCP' origCPI
                      forM_ mbOrigStdinEnc $ hSetEncoding stdin
                      )
              | otherwise = id
          fixOutput
              | setOutput = bracket_
                  (do
                      setConsoleOutputCP' cp
                      hSetEncoding stdout expected
                      hSetEncoding stderr expected
                      )
                  (do
                      setConsoleOutputCP' origCPO
                      forM_ mbOrigStdoutEnc $ hSetEncoding stdout
                      forM_ mbOrigStderrEnc $ hSetEncoding stderr
                      )
              | otherwise = id

      case (setInput, setOutput) of
          (False, False) -> return ()
          (True, True) -> warn ""
          (True, False) -> warn " input"
          (False, True) -> warn " output"

      fixInput $ fixOutput inner
    where
      warn typ = when chatty $ hPutStrLn stderr $ concat
          [ "Setting"
          , typ
          , " codepage to " ++ show cp
          ]

codePageEncoding' :: (CodePage -> TextEncoding) -> CodePage -> TextEncoding
#ifdef WINDOWS
codePageEncoding' _ = codePageEncoding
#else
codePageEncoding' = id
#endif

setConsoleCP', setConsoleOutputCP' :: CodePage -> IO ()
#ifdef WINDOWS
setConsoleCP'       = setConsoleCP
setConsoleOutputCP' = setConsoleOutputCP
#else
setConsoleCP'       _ = return ()
setConsoleOutputCP' _ = return ()
#endif
