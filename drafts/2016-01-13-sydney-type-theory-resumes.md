---
title: Sydney Type Theory resumes for 2016
tags: event, meetup, type theory
location: Sydney, New South Wales
excerpt: 
  Sydney Type Theory meetup has kicked off 2016 with a review of
  material we've covered so far.
---

The [Sydney Type Theory meetup][1] kicked off 2016


Type theory arose circa 1901 (Russell) and became a discipline in its
own right circa 1930s (Church, etc.) It's now probably more aligned
with computer science than mathematics and, being all programmers,
we'll be focusing on type theory as it pertains to programming and
programming languages.

We're going through PFPL as our first challenge, tackling
approximately a chapter a week. In this session we'll be skipping the
preliminary material and covering chapters 4-7.

Chapters 1-3 define the framework in which the rest of the book is
expressed.

Chapters 4-7 describe a very simple programming language with a
statics (type system), dynamics (evaluation), type safety (properties
which relate the statics and dynamics), and some alternative dynamics.

Chapter 4 begins by introducing a tiny language of expressions called
$\mathbf{E}$: it has numbers, strings, a few operations, and let
expressions. We introduce a typing judgment and the rules allowing us
to form these judgments. It should be the case that non-drunk,
non-insane people cannot argue with the these rules.

The typing rules contain two different things that we can call
"assumptions": the context of the typing judgment and the assumptions
of inference rules. Getting these two levels of object and meta
assumptions straight can take some puzzling but it's absolutely
critical to make sure you understand this point before continuing.

The most important typing rule for focus on for understanding
$\mathbf{E}$ is that for `let`:

$$\frac{
\Gamma \vdash e_{1}:\tau_1
\Gamma,x:\tau_1 \vdash e_{2}:\tau_2
}{
\Gamma \vdash let(e_{1};x.e_{2}):\tau_2
}$$

Syntax directed rules like these (ones in which a single rule applies
to any particular piece of syntax) can be interpreted as a search
procedure.

Unicity for typing: in every context, any expression has at most one
type.

Inversion for typing: ...

The structural property weakening allows us to have a bigger context
than we happen to need at any time. This means that merely knowing
something about `y` which does not appear in `x` doesn't preclude us
from reasoning about `x`.

The substitution lemma is the thing that gives meaning to our
variables. This is the sense in which they are variables
(mathematical) rather than something like C "variables".

The decomposition lemma is, in a sense, the inverse of the
substitution lemma. If we have some expression $e'$ with some
sub-expression $e$ which has type $\tau$ then we are allowed to pull
that subexpression out and put a new variable in its place.

$$\frac{
\Gamma \vdash plus(1;1):num
}{
\Gamma,x:num \vdash plus(x;x):num
}$$

Next week we'll look at chapter 5 which describes a *structural
semantics*, *contextual dynamics*, *equational dynamics*.

[1]: https://www.meetup.com/Sydney-Type-Theory/
[2]: https://www.cs.cmu.edu/~rwh/plbook/2nded.pdf
