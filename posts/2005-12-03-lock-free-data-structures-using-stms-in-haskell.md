---
title: Lock-Free Data Structures using STMs in Haskell
tags: reading, papers, haskell
---

[Lock-Free Data Structures using STMs in Haskell][0] Anthony Discolo,
Tim Harris, Simon Marlow, Simon Peyton Jones, and Satnam Singh.
Submitted to FLOPS'06.

An interesting paper demonstrating the use of STM (and it's superiority
over explicit locking) in Haskell. The authors present two Haskell
implementations of the `ArrayBlockingQueue` class from Java: one using STM
and the other explicit locking. Not only did the version using STM
outperformed the explicit locking version consistently in the multi-CPU
benchmarks, but the STM code is *much* simpler.

Previously discussed here at [1][1] and [2][2], and on [LtU][3].

[0]: http://research.microsoft.com/%7Esimonpj/papers/stm/lock-free.htm
[1]: /2005/composable-memory-transactions/
[2]: /2005/more-on-composable-memory-transactions/
[3]: http://lambda-the-ultimate.org/node/1151
