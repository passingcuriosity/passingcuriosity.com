---
title: Literate Haskell
---
Last night I spent a while wrestling with latex, vim, make and ghc as part of my source-code reorganisation. One of the things I've been wanting for quite a while, is a vim syntax highlighting mode that understands the LaTeX literate Haskell syntax and displays both the Haskell code and the LaTeX code properly. Last night, I finally got it to work, though it has taken a bit of hackery in my Haskell files: I need to have a comment in every file so that the highlighter can detect the LaTeX.

This is a problem as the LaTeX detection heuristic appears to be:<ol><li>Look for a \documentclass macro (the begining of a LaTeX document); or </li><li>Look for a LaTeX command; or</li><li>Assume that the "literate" part is not LaTeX</li></ol> Needless to say, this is rather annoying as I'm writing a program with a number of modules (and thus a number of files) and want to produce a single document from them. To ameliorate this problem, I just make sure to include a LaTeX-style comment on the first line of every literate Haskell file and use a few macros to do include imported modules into the LaTeX document.

I'm also using the listings package to pretty-print the Haskell code and my next goal is to use its custom keyword handling capabilities to <emph>automatically</emph> include the modules that we import (if they exist in our source tree and are LaTeX-style literate code).
