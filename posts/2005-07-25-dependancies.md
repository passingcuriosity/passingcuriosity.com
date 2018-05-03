---
wordpressid: 1447
layout: post
title: Dependancies...
wordpressurl: http://passingcuriosity.com/2005/dependancies/
---
I'm currently experimenting with <a href="http://www.cse.unsw.edu.au/~dons/hs-plugins/">hs-plugins</a>, a library which supports dynamically loading plugins in Haskell. Or, rather, I would be if I could manage to sort out the various hassles I'm encountering in installing it. 

The only problem is that <span style="font-family: monospace;">hs-plugins</span> depends on <span style="font-family: monospace;">Language.Haskell.Hsx</span>. This would be fine, if it came in the GHC distribution. Alas it doesn't, so I've had to <a href="http://www.cs.chalmers.se/~d00nibro/haskell-src-exts/">find</a> and install it myself. This isn't too much trouble - I have, after all, been using UNIX-like systems for at least 7 years now. The only bump in this stretch of the road is that <span style="font-family: monospace;">Language.Haskell.Hsx</span> depends on <a href="http://haskell.org/happy/#download"><span style="font-family: monospace;">Happy</span></a>.

Haskell is getting to the point where it could <emph>really</emph> do with a <a href="http://www.cpan.org/">CPAN</a>-style package archive, if only because introducing it now will be so much easier than later when it is actually necessary. <a href="http://www.haskell.org/cabal">Cabal</a> already provides these sorts of features (dependancy chasing, etc).
