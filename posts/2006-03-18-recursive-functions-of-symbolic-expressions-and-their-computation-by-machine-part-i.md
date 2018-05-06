---
title: Recursive functions of symbolic expressions and their computation by machine, Part I
---
<a href="http://www-formal.stanford.edu/jmc/recursive.pdf">Recursive functions of symbolic expressions and their computation by machine, Part I</a> by John McCarthy.

A milestone paper in computer science, this is one of, if not <b>the</b>, first LISP paper. This contains one of the earliest descriptions of garbage collection, functional programming and several other topics. It doesn't contain anything particularly enthralling for modern readers, but that is due to the fact that everything that <emph>is</emph> in it has been absorbed into every aspect of modern computer science.

One thing that I found interesting was the syntax given for conditional expressions:
<code>(p<sub>1</sub> &rarr; e<sub>1</sub>, &hellip;, p<sub>n</sub> &rarr; e<sub>n</sub>)</code> where <emph>p<sub>i</sub></emph> is a propositional expression (one that is either true of false) and <emph>e<sub>i</sub></emph> is an arbitrary expression. The value of a conditional expression is that of the first <emph>e<sub>i</sub></emph> such that <emph>p<sub>i</sub></emph> is true (or is undefined).

This is quite similar to the description of structures in the &rho;-calculus given in <a class="title" href="http://troacss.blogspot.com/2006/02/matching-power-by-cirstea-h-kirchner-c.html">Matching Power</a>.

<a href="http://doi.acm.org/10.1145/367177.367199">ACM</a> | <a href="http://dx.doi.org/10.1145/367177.367199">DOI</a> | <a href="http://www.citeulike.org/article/499736">CiteULike</a>
