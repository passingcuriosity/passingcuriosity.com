---
title: Annoyances of the Feature Kind
tags: haskell, functional programming, data structures
---

Yesterday, I finally sat down and wrote a priority queue (a [heap][1] to those
in the know) to store the formulae on a branch that still need to be processed.
Chris Okasaki (in his book [Purely Functional Data Structures][2]) defines the
interface of a heap as follows:

[1]: http://en.wikipedia.org/wiki/Heap
[2]: http://www.amazon.com/dp/0521663504/

<code><span class="keyword">class</span> Heap h <span class="keyword">where</span><code style="border: none;">    empty     <span class="keyword">::</span> (<span class="keyword">Ord</span> a) <span class="keyword">=&gt;</span> h a
  isEmpty   <span class="keyword">::</span> (<span class="keyword">Ord</span> a) <span class="keyword">=&gt;</span> h a <span class="keyword">-&gt;</span> Bool

  insert    <span class="keyword">::</span> (<span class="keyword">Ord</span> a) <span class="keyword">=&gt;</span> a <span class="keyword">-&gt;</span> h a <span class="keyword">-&gt;</span> h a
  merge     <span class="keyword">::</span> (<span class="keyword">Ord</span> a) <span class="keyword">=&gt;</span> h a <span class="keyword">-&gt;</span> h a <span class="keyword">-&gt;</span> h a

  findMin   <span class="keyword">::</span> (<span class="keyword">Ord</span> a) <span class="keyword">=&gt;</span> h a <span class="keyword">-&gt;</span> a
  deleteMin <span class="keyword">::</span> (<span class="keyword">Ord</span> a) <span class="keyword">=&gt;</span> h a <span class="keyword">-&gt;</span> h a
</code></code>

This is a problem (in my opinion) in that it requires that heaps (all heaps) be
over ordered data-types. The entire *reason* that I am attempting to extend the
heap is to remove the ordering from the contained type, and use a priority type
instead. Thus we are no longer saying that, for example, `? < ?` but that
`(priority ?) < (priority ?)`.

I'll be the first to admit that this isn't much of a distinction to make
(especially from a pragmatic "programmer" point of view), but it is a nicer
approach.

I got around this problem, such as it is, by modifying my Prioritised class to
have a function to inject values into my Priority type and one to extract them.
The heap then is a type synonym:

<code><span class="keyword">type</span> PriorityHeap t <span
class="keyword">=</span> LeftistHeap (Priority t)</code>

with the restriction that you need to explicitly wrap values you insert into a
heap:

<code style="text-align: center;"><span class="keyword">let</span> h' <span
class="keyword">=</span> insert (priority v) h <span class="keyword">in</span>
...</code>

and unwrap those you get out:

<code style="text-align: center;"><span class="keyword">let</span> v <span
class="keyword">=</span> value <span class="keyword">$</span> findMin h' <span
class="keyword">in</span> ...</code>

Not terribly inconvenient, but not ideal. I'm fairly sure that this could be
solved with some functional dependencies magic, but the current state of
affairs is good enough to be getting on with.
