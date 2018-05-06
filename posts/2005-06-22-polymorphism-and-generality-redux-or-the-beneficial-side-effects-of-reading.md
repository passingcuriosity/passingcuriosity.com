---
title: Polymorphism and Generality Redux; or The Beneficial Side Effects of Reading
---
I've started looking at how to code my backtracking monad (the framework that will provide the branching, failure, etc within which the tableau method for each logic will be implemented). To try to get a handle on the subject, I've started looking at <a href="http://portal.acm.org/citation.cfm?id=351258" style="font-style: italic;">Deriving Backtracking Monad Transformers</a> by Ralf Hinze.

Section 3 of the paper defines an abnormal termination (i.e. exceptions) monad and 3.1 begins to define a transformer to extend an arbitrary monad with exceptions. As part of this, it uses an existentially quantified type variable in the definition of a type constructor (for <span style="font-family: monospace;">:&gt;&gt;=</span>). Needless to say, the similarity to my previous attempts to make polymorphic  formulae was obvious.

Although I'm still, by any meaningful standard, a Haskell novice and haven't looked at this extension before, I've managed to extend that example (by restricting the type variable to members of my <span style="font-family: monospace;">Formula</span> type class) and Hey, Presto! I can now create polymorphic types representing my inter-mixable logical languages.

As an example, I've got a type <span style="font-family: monospace;">PC</span> for the propositional calculus, and a type < span style="font-family: monospace;" >Modal</span> for alethic modal logic. The connectives of each language can take formulae of either type (or both) as parameters, and everything Just Works<sup>TM</sup>.

Polymorphism, they way it was intended.
