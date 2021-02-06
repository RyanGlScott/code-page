### 0.2.1 [2021.02.06]
* Allow building with `base-4.15` (GHC 9.0) or later on Windows.

## 0.2 [2018.11.13]
* Rename the `withCodePageVerbosity` function to `withCodePageOptions` to
  reflect the fact that its first argument is now an `Options` data type
  instead of just a `Bool` to represent its verbosity. (The ability to
  configure verbosity is now controlled through the `chatty` field of
  `Options`.)
* On non-Windows OSes, `withCodePage` (and related functions) now make a best
  effort guess in converting the supplied `CodePage` to a `TextEncoding` and
  adjusing the current `TextEncoding` to that one. (For instance, `withCP65001`
  will adjust the current `TextEncoding` to be `utf8` on non-Windows OSes.)
  If the supplied `CodePage` does not map to a known `TextEncoding`, these
  functions will error at runtime on non-Windows OSes.

  This is a departure from the previous major version of `code-page`, where
  these functions did not do anything at all on non-Windows OSes. If you
  would like to recover this old behavior, use
  `withCodePageOptions defaultOptions{nonWindowsBehavior = NonWindowsDoNothing}`.
* `withCodePage` and friends now change the locale encoding (on GHC 7.4 or later)
  in addition to the encodings for `stdin`, `stdout`, and `stderr`.
* Add `withCP1252` and `cp1252` for the Latin1 code page.
* Add a `System.IO.CodePage.Internal` module that contains certain internal
  details (such as the constructors of `Options` and `NonWindowsBehavior`).

### 0.1.3 [2017.03.15]
* Fix the build on GHC 7.8 and older

### 0.1.2 [2017.02.20]
* Squash minor bug in fixCodePage (the same bug reported in
  https://github.com/commercialhaskell/stack/pull/3002)

### 0.1.1 [2016.11.09]
* Fix the build on non-Intel architectures (thanks, erikd!)

## 0.1 [2016.09.15]
* Initial commit.
