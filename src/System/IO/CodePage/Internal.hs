{-# LANGUAGE CPP #-}

{-|
Module:      System.IO.CodePage.Internal
Copyright:   (C) 2018 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Experimental
Portability: Portable

Various internals used by "System.IO.CodePage".

Note that this is an internal module, and as such, the API presented here is
not guaranteed to be stable, even between minor releases of this library.
-}
module System.IO.CodePage.Internal where

import           System.IO (TextEncoding, latin1, utf8, utf16le, utf16be, utf32le, utf32be)

#ifdef WINDOWS
import qualified System.Win32.CodePage as Win32 (CodePage)
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

-- | The Latin1 code page.
cp1252 :: CodePage
cp1252 = 1252

-- | Options that specify how 'withCodePage' and friends should work.
data Options = Options
  { chatty :: Bool
    -- ^ If 'True', emit a warning to @stderr@ indicating that the code page has
    -- been changed. If 'False', don't emit any warnings.
  , nonWindowsBehavior :: NonWindowsBehavior
    -- ^ Configures how 'withCodePage' and friends should work on non-Windows
    --   operating systems.
  }

-- | The default 'Options':
--
-- @
-- 'Options'
-- { 'chatty' = 'False'
-- , 'nonWindowsBehavior' =
--     'nonWindowsFallbackCodePageEncoding' 'defaultFallbackCodePageEncoding'
-- }
-- @
defaultOptions :: Options
defaultOptions = Options
  { chatty = False
  , nonWindowsBehavior =
      nonWindowsFallbackCodePageEncoding defaultFallbackCodePageEncoding
  }

-- | Specifies how 'withCodePage' and friends should work on operating systems
-- other than Windows.
data NonWindowsBehavior
 = NonWindowsDoNothing
   -- ^ Don't do anything at all on non-Windows OSes.
 | NonWindowsFallbackCodePageEncoding (CodePage -> TextEncoding)
   -- ^ On non-Windows OSes, change the 'TextEncoding' by converting the
   --   'CodePage' argument to a 'TextEncoding' using the supplied function.

-- | Don't do anything at all on non-Windows OSes.
nonWindowsDoNothing :: NonWindowsBehavior
nonWindowsDoNothing = NonWindowsDoNothing

-- | On non-Windows OSes, change the 'TextEncoding' by converting the
-- 'CodePage' argument to a 'TextEncoding' using the supplied function.
nonWindowsFallbackCodePageEncoding
  :: (CodePage -> TextEncoding) -> NonWindowsBehavior
nonWindowsFallbackCodePageEncoding = NonWindowsFallbackCodePageEncoding

-- | Provides a best-effort attempt to convert a 'CodePage' to a 'TextEncoding'
-- on non-Windows OSes. Errors if given a 'CodePage' that it doesn't know how
-- to convert.
defaultFallbackCodePageEncoding :: CodePage -> TextEncoding
defaultFallbackCodePageEncoding cp
  | cp == cp65001
  = utf8
  | cp == cp1200
  = utf16le
  | cp == cp1201
  = utf16be
  | cp == cp12000
  = utf32le
  | cp == cp12001
  = utf32be
  | cp == cp1252
  = latin1
  | otherwise
  = error $ "Don't know fallback text encoding for CP" ++ show cp
