---
wordpressid: 1525
layout: post
title: Wanted: LaTeX packages
tags: LaTeX, wishlist
wordpressurl: http://passingcuriosity.com/2006/wanted-latex-packages/
---

I've got a hankering to dig out my incomplete thesis and polish it off as an
excuse to *really* get up to speed with [LaTeX](http://www.tug.org/). As such,
I'm looking for LaTeX packages that accomplish a number of tasks:

- *Unicode and symbols*

    The main thing I'm after is a package that will allow me to `&and;` instead
    of `\wedge` in my LaTeX source and have it Just Work (TM). A single package
    that provides consistent symbol handling would be nice too.

- *Meta-data support*

      While [hyperref][] can be used to add PDF meta-data, and [xmpincl][] can
      include an RDF license when using `pdftex`, you still need to do it all
      yourself. It'd be nice if there was a module that would do it for you.

- *Font handling*

    A single, consistent way to handle real fonts. I like *Computer Modern* as
    much as the next person, but once in a while I'd like to be able to use
    another font without having to wrestle with TeX.

I'll admit that most of my desire for packages to address these issues is
probably an issue more of my knowledge of existing code than that code's
availability, but it'd be nice if there was a little more consistency in my
LaTeX environment and a *lot* more consistency in Google's LaTeX search
results.

[hyperref]: http://www.tug.org/applications/hyperref/
[xmpincl]: http://www.ctan.org/tex-archive/macros/latex/contrib/xmpincl
