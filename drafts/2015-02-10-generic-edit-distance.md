---
title: Generic Edit Distances
strapline: Generalising string edit distance for abstract data types.
tags: haskell, functional programming, algorithms, abstraction
location: Sydney, New South Wales
excerpt: 
  I've been working on a project that needs to compute the difference between
  two data structures. This post describes a generic algorithm to compute such
  differences.
---

I've been working on a problem which requires computing the difference between
two sequences of values and there are a number of good reasons to prefer
smaller acceptable solutions to larger ones. I seem to recall university
classes about a wealth of material for solving this problem for sequences of
characters (i.e.  for strings) but can't, off the top of my head remember one
for more complete data types. So I'll generalise the algorithm I have to make
it work in the case I care about.

Levenshtein edit distance
=========================



