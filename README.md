# `code-page`
[![Hackage](https://img.shields.io/hackage/v/code-page.svg)][Hackage: code-page]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/code-page.svg)](http://packdeps.haskellers.com/reverse/code-page)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]
[![Linux build](https://github.com/RyanGlScott/code-page/workflows/Haskell-CI/badge.svg)](https://github.com/RyanGlScott/code-page/actions?query=workflow%3AHaskell-CI)
[![Windows build](https://ci.appveyor.com/api/projects/status/kaxqsgm2xx66l2q5?svg=true)](https://ci.appveyor.com/project/RyanGlScott/code-page)

[Hackage: code-page]:
  http://hackage.haskell.org/package/code-page
  "code-page package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"

This library provides two modules:

* `System.IO.CodePage`: a cross-platform module that exports functions which adjust code pages on Windows, and do nothing on other operating systems.
* `System.Win32.CodePage`: On Windows, this exports functions for getting, setting, and analyzing code pages. On other operating systems, this module exports nothing.
