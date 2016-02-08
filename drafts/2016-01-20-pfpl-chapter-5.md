---
title: Sydney Type Theory covers PFPL chapter five
tags: event, meetup, learning, type theory, programming languages, reading
location: Sydney, New South Wales
excerpt: 
  The Sydney Type Theory meet-up recapped chapter five of Practical
  Foundations for Programming Languages.
---

Skipping through to inference rules and reading *them* and referring
to the text as needed to clarify.

What does a program mean? We'll start by defining a transition system
we can use to evaluate a program - the meaning of the program is the
value it evaluates to. We have a transition system formed by states
$s$, with transitions between them $s \mapsto s'$, and some set of
which are terminating $s \: final$.

We can take any such transition system and build a system of
transition *sequences* by taking the closure over the $\mapsto$
relation:

$$\frac{}{s \mapsto^{*} s}$$

$$\frac{s \mapsto s' \qquad s' \mapsto^{*} s''}{s \mapsto^{*} s''}$$

We can also, if we choose, define an *n-times iterated* system which
tracks the number of transitions taken between two states:

$$\frac{}{s \mapsto^{0} s}$$

$$\frac{s \mapsto s' \qquad s' \mapsto^{n} s''}{s \mapsto^{n+1} s''}$$

Such a transition system is defined inductively in a similar manner to
the typing rules:

$$\frac{
e_{1} val \qquad e_{2} \mapsto e_{2}'
}{
plus(e_{1};e_{2}) \mapsto plus(e_{1};e_{2}')
}$$

The choices we make in designing rules changes the semantics of the
language.

$$num[n] \: val$$

$$str[s] \: val$$

By including or ignoring the fragments wrapped in $[...]$ we
can have call-by-name or call-by-value semantics for `let`:

$$\left[\frac{
e_1 \mapsto e_{1}'
}{
let(e_1;x.e_2) \mapsto let(e_{1}';x.e_2)
}\right]$$

$$\frac{
[e_{1} \; val]
}{
let(e_{1};x.e_{2}) \mapsto [e_{1}/x]e_{2}
}$$

We can interpret a complete set of these rules for the language as
defining a logic program which computes the value of a program. This
results in a two dimensional structure: a sequence of nodes linked by
the $\mapsto$ relation, each of which is the root of a tree of
inferences which justify the transition.

The Finality of Values and Determinacy lemmas (5.2 and 5.3) ensure
that our system behaves sensibly ($e \mapsto e'$ and $e \mapsto e''$
implies that $e'$ and $e''$ are $\alpha$-equivalent and that values
are, indeed, values).

Type Safety
===========

Statics and dynamics work well together and "nothing weird happens":

1. If $e : \tau$ and $e \mapsto e'$ then $e' : \tau$.

2. If $e : \tau$, then either $e \; val$, or there exists $e'$ such
   that $e \mapsto e'$.

The first part is often called preservation -- when we evaluate an
expression, the type doesn't change -- and the second is progress --
when an expression has a type it's either a value or we can evaluate
it. It follows from these conditions that a *stuck* expression (one
which is *not* a value but cannot be evaluated) is not well-typed.

We prove *preservation* by cases: for each rule in the $\mapsto$
transition relation, use inversion of the typing rules on original
expression, and check that the result expression has the same type.

$$\frac{
e_{1} \mapsto e_{1}'
}{
plus(e_{1};e_{2}) \mapsto plus(e_{1}';e_{2})
}$$

The only rule which can type the expression $plus(e_{1};e_{2})$
assigns the type $num$:

$$\frac{
\Gamma \vdash e_1 : num \qquad \Gamma \vdash e_2 : num
}{
\Gamma \vdash plus(e_1;e_2) : num
}$$

The same rule will apply to $plus(e_{1}';e_{2})$ and assign the type
$num$ to it as well.

We prove *progress* is slightly trickier: we proceed by induction on
the typing derivation.

$$\frac{
\Gamma \vdash e_1 : num \qquad \Gamma \vdash e_2 : num
}{
\Gamma \vdash plus(e_1;e_2) : num
}$$

The induction hypothesis tells us that $e_1$ and $e_2$ are either a
value or can be evaluated to values. We can show that $e_1$ can be
evaluated, or that $e_2$ can be evaluated, or that both are values and
we can evaluate $plus(e_1;e_2)$.

If we have some form of run-time errors (division by zero, for
example) we can extend these semantics with an $err$ judgment which
represents stopping with a run-time error. The lemmas and theorems can
then be restated replacing "results in a value" with "results in a
value or an error".
