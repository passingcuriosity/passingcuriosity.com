---
wordpressid: 1425
layout: post
title: Data Structures
wordpressurl: http://passingcuriosity.com/2005/data-structures/
---
I'm getting to the point where I'll have to start thinking about the data structures used to implement my tableaux. They need to be as small as possible and allow fast membership testing and updating. As my current goal is to accept a formula to prove and produce either a counter-model or a "yes" as output. As such, I will only ever need access to those parts of the tree on the path from the current node, to the root. The tree is, essentially, the progress of the computation backtracking over time.

As such, I'm looking forward to trying to write the run-time (once I've finished figuring out how the logic-specific code the DSL will be transformed into will work). It will require a bit of input handling (in the IO monad), parsing (in the Parser monad) and backtracking (in some backtracking monad) along with some state (perhaps a State monad, perhaps built into the backtracking monad) and a whole bunch of lazy computation (the actual tableau rules).

Unless I am overlooking something (I'm sure I am and I imagine that Raj and Pietro agree), my main data structure will be...
 <span style="font-weight: bold;">a list</span>. The main rationale behind this decision (rather than trying to build some sort of tree structure, or some bizarre nested set-like structure) is that, as I'm merely trying to build a model, the tree itself is expressed in the computation (or, rather, its backtracking). Furthermore, as I'm working with labelled tableaux (with an explicit modal relation), all I need to construct a model once I've found an open branch is the list of formul&aelig; (and relations) from the end of that branch, to the root of the tree.

In Haskell, lists act like (and probably are) singly linked lists of cons cells with a pointer to the item, and one to the rest of the list. This means that you can add stuff to the front of a list without modifying the original list. This is rather helpful as it means that we can add junk onto one end of a list, which is what we'll be doing with tableaux (adding things to the end of the branch), and you can have multiple lists sharing the same tail (to give an right-way-up tree structure, like an upside-down tableau or a sequent proof). As we're going to be doing backtracking (because we can through away everything we don't need to write a model), and we've got a ready-made tree structure that can be extended at the leaves, there is a very simple way to code it all up: as a monad (for the backtracking) over the list pointers. 

Using a monad (probably a MonadPlus) to implement backtracking will allow us to express branching naturally (using <tt>mzero</tt> "fail" on branch closure and <tt>mplus</tt> to do the "branching" evaluation) and if we add in a little state to each branch (a pointer to the current head of the list, for instance) we have a backtracking model search procedure. All that remains is to implement it (no doubt discovering that my understanding of monads is completely wrong) and find out which mountains I need to move to get this approach to work for the non-simple case...
