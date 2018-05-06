---
title: Integrating support for undo with exception handling
tags: programming, cs, papers, reading, haskell
---

<a class="title" href="http://research.microsoft.com/research/pubs/view.aspx?tr_id=845">Integrating support for undo with exception handling</a> by Avraham Shinnar; David Tarditi; Mark Plesko and Bjarne Steensgaard.

This paper appears to have as its object a solution to similar problems
as those addressed by software transactional memory, i.e. maintaining
memory consistency. Where <acronym title="software transactional
memory">STM</acronym> provides transactions that can be used to
implement multi-threaded programmes much more simply that traditional
explicit locking approaches, this paper uses a similar mechanism to
handle unexpected failures (uncaught exceptions) and roll-back the
effects of the failed code.

The authors describe their implementation of an undo mechanism in
Bartok, an experimental optimising <acronym title="C-Sharp">C#</acronym>
compiler and <acronym title="Common Language Runtime">CIL</acronym>
runtime. Their system extends <acronym title="C-Sharp">C#</acronym> with
a <code>try_all</code> block which acts as a catch-all exception
handler. If an exception propagates is raised within a
<code>try_all</code> block and is not handled before it reaches the
scope of the block, then the roll-back mechanism is engaged.

The main difference between Haskell's <acronym title="software
transactional memory">STM</acronym> is the goal: where <acronym
title="software transactional memory">STM</acronym> is concerned with
maintaining memory consistency in the face of concurrently executing
threads, this system is concerned with simplifying error handling and
recovery.

<a href="http://scholar.google.com/scholar?hl=en&lr=&safe=off&cluster=14653294056375713537">Google</a> |
<a href="http://www.citeulike.org/article/556290">CiteULike</a> |
<a href="http://lambda-the-ultimate.org/node/446">LtU</a>
