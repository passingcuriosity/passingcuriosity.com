---
title: Threads Cannot Be Implemented As a Library
---

<span class="title">Threads Cannot Be Implemented As a Library</span> by Hans-J. Boehm. In PLDI'05. <a href="http://www.hpl.hp.com/techreports/2004/HPL-2004-209.pdf">(PDF)</a>

I read this paper for a presentation I had to do for <a href="http://cs.anu.edu.au/~Steve.Blackburn/teaching/comp4700/">Advanced Run Time Systems</a> last year. Using <a href="http://www.llnl.gov/computing/tutorials/pthreads/">Pthreads</a> as an example, Boehm points out that implementing threads purely as a library can result in a number of errors which are difficult to find and resolve. The paper presents three examples: <ul><li><strong>concurrent modification</strong> &mdash; where races are introduced by optimisations ignorant of concurrency requirements; </li><li><strong>rewriting adjacent data</strong> &mdash; where race conditions are introduced when variables protected by different locks are packed into the same memory location;</li><li><strong>register promotion</strong> &mdash; where register promotion optimisations can modify protected variables outside of an atomic region.</li></ul>As is mentioned in the <acronym title="The Programming Languages Weblog">Lambda the Ultimate</acronym> discussion (linked below), this paper doesn't describe anything terribly new but it was an interesting read.

<a href="http://portal.acm.org/citation.cfm?doid=1065010.1065042">ACM</a> |
<a href="http://scholar.google.com/scholar?cluster=12433137887989415736">Google</a> |
<a href="http://www.citeulike.org/article/230206">CiteULike</a> |
<a href="http://lambda-the-ultimate.org/node/950">LtU</a>
