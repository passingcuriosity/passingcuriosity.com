---
title: Relational Algebra in Haskell
---

I've just spoken to <a href="http://thatlogicblog.blogspot.com/">Jon</a> about the problems and ideas I have <a href="http://labelledtableaux.blogspot.com/2005/07/thoughts-on-hypergraphs.html">posted</a> about previously. One of the things I came away with is the suggestion that I might be thinking about  relational algebras (perhaps an <a href="http://en.wikipedia.org/wiki/Algebra_over_a_set">algebra over a set</a>) rather than hypergraphs. I'm not entirely sure that this is the case (the "hypergraph" Wikipedia article above seems to describe what I'm thinking of), but the "algebra over a set" article is certainly applicable as well).

As a result, I've been able to dig up a couple of things that look like they might help me implement support for generic frames (i.e. those with an arbitrary number of <span style="font-style: italic;">n</span>-relations for arbitrary values of <span style="font-style: italic;">n</span> greater than 0).

While I've seen <a href="http://weblogs.asp.net/brianbec/articles/246392.aspx">A Relational Algebra Simulator in Haskell</a> before, it didn't occur to me that it might be applicable to my problem. <a href="http://www2-data.informatik.unibw-muenchen.de/relmics/tools/RATH/">RATH - Relation Algebra Tools in Haskell</a>, however is new to me and looks like it provides a complete library (rather than the example program of the first link).

Whether I have enough time to catch up on my course-work, finish my system (as it stands), write a thesis <emph>and</emph> learn enough about this topic to extend the system using such a generalisation remains to be seen.

Another possibly useful generalisation might be to extend the concept of "relation" from sets of tuples to mutisets (or bags) of tuples, thereby allowing relations in a frame to be <a href="http://en.wikipedia.org/wiki/Multigraph">mutigraphs</a> (i.e. graphs allowing multiple edges between a given pair of vertices). The <a href="http://www.haskell.org/ghc/docs/latest/html/libraries/fgl/Data.Graph.Inductive.html">Data.Graph.Inductive</a> library supports labelled edges which suggests that it might also support multigraphs to some degree (if edges with non-identical labels are non-identical, then you can simulate an unlabelled multigraph by  labelled each edge with an index).

I'd like to write a generic graph library with support for multigraphs and [uniform] hypergraphs, possibly by extending Data.Graph.Inductive, but this will probably wind up being pushed back until the Christmas break.
