---
wordpressid: 1445
layout: post
title: More on Literate Haskell
wordpressurl: http://passingcuriosity.com/2005/more-on-literate-haskell/
---

I've been having fun extending my environment for literate Haskell programming
in LaTeX. While I still haven't looked at the suggestion my previous post
provoked (though I'll get round to it eventually), I have been taking the
opportunity to learn some of the more *program-y* things one can do in
LaTeX.

The macro I use for the title of each module (called `\module` funnily enough)
now checks to see if it is a sub-module (i.e. contains a '.' in its name) and
makes it a `\subsection` if it does. For now, this will be enough, but later on
there will be some configurability to this to make sure that we don't typeset
modules as sub-sections of random other modules.

I'm also planning on making the pretty-printing code build a list of imported
modules as it typesets the Haskell and then include the appropriate files (I've
got an `\import` macro all ready and
waiting) afterward.

I like playing with LaTeX.
