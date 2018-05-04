---
title: Stupidity or How Dumb Can I Get
tags: haskell, plugins
---

I've just been trying to install <a
href="http://www.cse.unsw.edu.au/~dons/hs-plugins/"><span style="font-family:
monospace;">hs-plugins</span></a> on my iBook with <a
href="http://www.haskell.org/ghc/">GHC</a> 6.4. Sorting out the dependancies,
getting stuff to build and getting the libraries installed has been
ridiculously complex.

Now that I've got it built and installed (part of which required using `hugs`
because `runghc` refused to run the setup program), it doesn't appear to have
registered the package, which means I need to manually provide the details to
`ghc` every time I compile some code using `System.Plugins` (rather than just
tell the compiler to use the package: `-package plugins`).

I need to figure out what it going wrong with my environment and fix it.
Perhaps by tearing some capabilities out...

**Update:** I've got that problem fixed (`make install` doesn't appear to
update the package database).  Now all I need to do is work out a way to be
able to specify the data-types in the plug-ins.
