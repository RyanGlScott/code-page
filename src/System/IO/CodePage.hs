{-# LANGUAGE CPP #-}

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
      withCP65001
    , withCP1200
    , withCP1201
    , withCP12000
    , withCP12001
    , withCodePage
    , withCodePageVerbosity

    , CodePage
    , cp65001
    , cp1200
    , cp1201
    , cp12000
    , cp12001
    ) where

#ifdef WINDOWS
import           Control.Exception (bracket_)
import           Control.Monad (when)
import           Data.Foldable (forM_)
import           System.IO (hGetEncoding, hPutStrLn, hSetEncoding, stderr, stdin, stdout)
import qualified System.Win32.CodePage as Win32 (CodePage)
import           System.Win32.CodePage hiding (CodePage)
#else
import           Data.Word (Word32)
#endif

-- | A numeric type representing Windows code pages.
type CodePage =
#ifdef WINDOWS
  Win32.CodePage
#else
  Word32
#endif

-- | The UTF-8 code page.
cp65001 :: CodePage
cp65001 = 65001

-- | The UTF-16LE code page.
cp1200 :: CodePage
cp1200 = 1200

-- | The UTF-16BE code page.
cp1201 :: CodePage
cp1201 = 1201

-- | The UTF-32LE code page.
cp12000 :: CodePage
cp12000 = 12000

-- | The UTF-32BE code page.
cp12001 :: CodePage
cp12001 = 12001

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

-- | Sets the code page for an action as necessary.
withCodePage :: CodePage -> IO a -> IO a
withCodePage = withCodePageVerbosity False

-- | Sets the code page for an action as necessary. If the 'Bool' argument is 'True',
-- this function will emit a warning to @stderr@ indicating that the code page has
-- been changed. ('withCodePage' sets this argument to 'False'.)

-- Taken from the stack codebase
-- (https://github.com/commercialhaskell/stack/blob/21e517ba88b3c6bee475fb00ad95f280e7285a54/src/main/Main.hs#L82-L123)
-- which is under a 3-clause BSD license
withCodePageVerbosity :: Bool -> CodePage -> IO a -> IO a
#ifdef WINDOWS
withCodePageVerbosity chatty cp inner = do
    origCPI <- getConsoleCP
    origCPO <- getConsoleOutputCP
    mbOrigStdinEnc  <- hGetEncoding stdin
    mbOrigStdoutEnc <- hGetEncoding stdout
    mbOrigStderrEnc <- hGetEncoding stderr

    let setInput  = origCPI /= cp
        setOutput = origCPO /= cp
        fixInput
            | setInput = bracket_
                (do
                    setConsoleCP cp
                    hSetEncoding stdin expected
                    )
                (do
                    setConsoleCP origCPI
                    forM_ mbOrigStdinEnc $ hSetEncoding stdin
                    )
            | otherwise = id
        fixOutput
            | setOutput = bracket_
                (do
                    setConsoleOutputCP cp
                    hSetEncoding stdout expected
                    hSetEncoding stderr expected
                    )
                (do
                    setConsoleOutputCP origCPO
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
    expected = codePageEncoding cp
    warn typ = when chatty $ hPutStrLn stderr $ concat
        [ "Setting"
        , typ
        , " codepage to " ++ show cp
        ]
#else
withCodePageVerbosity _ _ inner = inner
#endif
