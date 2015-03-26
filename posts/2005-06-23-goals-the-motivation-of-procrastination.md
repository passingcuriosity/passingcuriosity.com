---
wordpressid: 1430
layout: post
title: Goals: the Motivation of Procrastination
tags: logic, haskell
wordpressurl: http://passingcuriosity.com/2005/goals-the-motivation-of-procrastination/
---

So far, I have:

- [written a parser][1] for my logic definition language;
- [solved][2] the [problem][3] I was having with polymorphism;
- [designed][4], in the large, my data structures; and
- [thought][5] about name for the project.

[1]: /2005/parsing-stuff-in-haskell/
[2]: /2005/polymorphism-and-generality-redux-or/
[3]: /2005/polymorphism-and-generality/
[4]: /2005/data-structures/
[5]: /2005/hello-ill-be-your-host-tonight-as-we/

We've now entered the mid-year holiday between the two semesters and I am
hoping to get a large amount of my implementation done during the break. On my
list of things to do are:

- define a state monad (to model the saturation of a branch);

- define a backtracking monad (to model the branching of non-deterministic
choices);

- find a way to compose them (perhaps by [deriving a monad transformer ][6]);

- finalise the structure of the logic modules;

- write a realistic (perhaps even production) parser and code generator for the
logics;

- learn to use [hs-plugins][7] to write dynamically loadable modules (and find
a way to make logic modules dynamically loadable); and

- implement some representative test logics (propositional calculus, K,
intuitionistic logic, something with transitive frames, etc).

I've got a lot of work ahead of me, but the next few weeks are going to be fun.
At the end of it, I will (I hope) have a running system to extend and improve
and a much greater knowledge of Haskell.

[6]: /2005/polymorphism-and-generality-redux-or/
[7]: http://www.cse.unsw.edu.au/~dons/hs-plugins/
