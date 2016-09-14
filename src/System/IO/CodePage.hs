{-# LANGUAGE CPP #-}

-- TODO: Docs
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
import           System.IO (hGetEncoding, hPutStrLn, hSetEncoding, stderr, stdin, stdout)
import qualified System.Win32.CodePage as Win32 (CodePage)
import           System.Win32.CodePage hiding (CodePage)
#else
import           Data.Word (Word32)
#endif

type CodePage =
#ifdef WINDOWS
  Win32.CodePage
#else
  Word32
#endif

cp65001 :: CodePage
cp65001 = 65001

cp1200 :: CodePage
cp1200 = 1200

cp1201 :: CodePage
cp1201 = 1201

cp12000 :: CodePage
cp12000 = 12000

cp12001 :: CodePage
cp12001 = 12001

withCP65001 :: IO a -> IO a
withCP65001 = withCodePage cp65001

withCP1200 :: IO a -> IO a
withCP1200 = withCodePage cp1200

withCP1201 :: IO a -> IO a
withCP1201 = withCodePage cp1201

withCP12000 :: IO a -> IO a
withCP12000 = withCodePage cp12000

withCP12001 :: IO a -> IO a
withCP12001 = withCodePage cp12001

withCodePage :: CodePage -> IO a -> IO a
withCodePage = withCodePageVerbosity False

-- TODO: Credit stack with this trick
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
                    mapM_ (hSetEncoding stdin) mbOrigStdinEnc
                    )
            | otherwise = id
        fixOutput
            | setInput = bracket_
                (do
                    setConsoleOutputCP cp
                    hSetEncoding stdout expected
                    hSetEncoding stderr expected
                    )
                (do
                    setConsoleOutputCP origCPO
                    mapM_ (hSetEncoding stdout) mbOrigStdoutEnc
                    mapM_ (hSetEncoding stderr) mbOrigStderrEnc
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
        , " codepage to UTF-8 (65001) to ensure correct output from GHC"
        ]
#else
withCodePage _ _ inner = inner
#endif
