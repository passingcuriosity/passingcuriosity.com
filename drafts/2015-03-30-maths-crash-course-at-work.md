---
title: A maths crash course at work
tags: work, mathematics, learning
location: Sydney, New South Wales
excerpt: 
  One of the things I like most about my present job is the opportunities it
  provides to learn new and revist old things. Lately we've been learning some
  maths.
---

# Numbers

- Counting is a fairly easy way to ground the existence of numbers and gives us
the natural numbers: $\mathbb{N}$. $0\in\mathbb{N}$ and
$\forall{}x\in\mathbb{N}. S(x)\in\mathbb{N}$.

- But inverses are useful properties to have. Extending $\mathbb{N}$ with
additive inverses (i.e. negative numbers) gives us the integers: $\mathbb{Z}$.

- Similarly, extending $\mathbb{Z}$ with multiplicative inverses (i.e.
fractions) yields the rational numbers: $\mathbb{Q}$.

Unfortunately $\mathbb{Q}$ does not contain all the numbers we might like;
there are some numbers that are *not* in $\mathbb{Q}$. To show this we need to
pick such a number and show that it isn't in $\mathbb{Q}$. One common example
is $\sqrt{2}$. We know this number exists by the [Pythagorean theorem][1]
applied to the unit triangle: $a^{2} + b^{2} = c^{2}$. When $a=1$ and $b=1$
then $c=\sqrt{2}$.

1. Suppose $\sqrt{2}$ is in $\mathbb{Q}$.

2. Then there exists some irreducible fraction $\frac{n}{m}\in\mathbb{Q}$ equal
to $\sqrt{2}$.

3. ...

4. Then $n|2$ and $m|2$, but we choose $\frac{n}{m}$ to be irreducible.
Contradiction! 

5. The assumption that $\sqrt{2}$ is in $\mathbb{Q}$ must be false.

So we know that $\mathbb{Q}$ has holes in it. Filling those holes results in
the real numbers $\mathbb{R}$

# Countability

We can call a set countable if we can establish a bijection (i.e. a one-to-one
mapping) with the natural numbers.

- $\mathbb{N}$ is trivial.

- $\mathbb{Z}$ is slightly more complex: we map zero to itself ($0 \mapsto 0$),
the positive integers to the even naturals ($x \mapsto 2x$), and the negative
integers to the odd naturals ($x \mapsto -2x - 1$).

- $\mathbb{Q}$ is more complex again: we arrange the rational numbers in a grid
and use a [space-filling curve][2] to traverse the numbers. Each number is then
mapped to its position in the sequence.

We can show that $\mathbb{R}$ (unlike $\mathbb{N}$, $\mathbb{Z}$, and
$\mathbb{Q}$) is not countable. Cantor's diagonalisation proof. TODO

I'm not sure how much I should care, but Mark also mentioned that some people
don't work with $\mathbb{R}$ but instead bolt $\sqrt{2}$ on the side of
$\mathbb{Q}$ with a [field extension][3] to get $\mathbb{Q}[\sqrt{2}]$.

# Floating point representations

# Measure theory

Both $[0,1]\in\mathbb{R}$ and $[0,2]\in\mathbb{R}$ have the same cardinality
(this must be the case as $x \mapsto 2x$ is a bijection), so cardinality is not
a useful way to compare intervals of $\mathbb{R}$. This is where [measure
theory][4] comes in.

TODO

[1]: https://en.wikipedia.org/wiki/Pythagorean_theorem
[2]: https://en.wikipedia.org/wiki/Space-filling_curve
[3]: https://en.wikipedia.org/wiki/Field_extension
[4]: https://en.wikipedia.org/wiki/Measure_%28mathematics%29
