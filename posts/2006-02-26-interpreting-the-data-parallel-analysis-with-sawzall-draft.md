---
title: "Interpreting the Data: Parallel Analysis with Sawzall (Draft)"
tags: papers, programming languages
---

<a class="title" href="http://labs.google.com/papers/sawzall.html" title="Interpreting the Data: Parallel Analysis with Sawzall (Draft)">Interpreting the Data: Parallel Analysis with Sawzall (Draft)</a>

While <a href="http://labs.google.com/papers/mapreduce.html" title="MapReduce: Simplified Data Processing on Large Clusters">MapReduce</a> and <a href="http://labs.google.com/papers/gfs.html" title="The Google File System">GFS</a> allow Google to use their massive computer <a href="http://labs.google.com/papers/googlecluster.html" title="Web Search for a Planet: The Google Cluster Architecture">clusters</a> effectively, the use of C++ can make programming for such a systems more difficult than it needs to be. Cue Sawzall, a new language that Google use to write distributed, parallel data-processing programs for use on their clusters. While the language isn't particularly attractive (I've never liked C-style syntax's), the approach is very interesting and the implementation issues they describe are enlightening.

<a href="http://lambda-the-ultimate.org/node/916">LtU</a> | <a href="http://www.citeulike.org/article/227597">CiteULike</a>
