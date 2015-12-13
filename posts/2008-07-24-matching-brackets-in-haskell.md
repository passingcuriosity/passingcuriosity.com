---
wordpressid: 155
layout: post
title: Matching Brackets in Haskell
wordpressurl: http://passingcuriosity.com/?p=78
---

I have no idea where I came across the link (someone's solution in
PHP?), but I stumbled on [A Programming Job Interview Challenge #13 -
Brackets][1] the other day. It's a pretty trivial problem but I was
bored tonight, so I hacked up a quick solution in [Haskell][2]. Haskell
is a statically typed, non-strict, purely functional programming
language that has whole great big piles of awesome built right in. What
follows is pretty routine, but in world dominated by the likes of Java,
C, .Net and the other misfits, Haskell is pretty strange so I'll try to
explain.

[1]: http://www.dev102.com/2008/07/21/a-programming-job-interview-challenge-13-brackets/
[2]: http://haskell.org/

As Haskell is a purely functional programming language, its functions
are first class (you can given them names, pass them around, create new
ones, etc., etc.) and they are functions in the mathematical sense (the
same values yield the same results, every single time). This latter
point implies that there is no mutable state (i.e. variables that can
change). While this might seem like a pretty big handicap, it actually
means that entire classes of bugs are impossible, that compilers and
optimisers can do more to your code, and too much other stuff to mention
here (and a super cool feature called *monads* lets us use mutable
variables even though there aren't any).

Enough about Haskell on with the show: given a *string of characters* we
need to determine if the brackets in it *are* or *are not* validly
nested. That is, we've need to define a function that takes a `String`
and returns a `Bool` (`True` when they are valid and `False` when they
are not). I used a pretty standard pattern in Haskell, whereby we use a
thin wrapper to provide a convenient interface to a worker function that
does the real work. We'll call the wrapper `check` and the worker
`check'` (again, this is a pretty standard naming convention in
Haskell).

Thinking about the problem a little, it should be pretty clear what
algorithm we need to use: for each character in the input:

 * If it is not a bracket, skip it.

 * If it is an opening bracket push its opposite onto a stack of
   expected closing brackets.

 * If it is a closing bracket, pop the value of the expected stack. If
   they are not equal, then the brackets are invalidly nested and you
   can stop now. Otherwise keep going.

When you've processed all of your input, the stack should also be empty
(otherwise you haven't seen a closing bracket you were expecting).

In Haskell, a `String` is a list of characters, so we'll use one
`String` as our input and one `String` as our stack. Our wrapper then
will need to take the input, and pass it and an empty stack (we aren't
expecting any brackets yet, after all) to our worker:

````{.haskell}
check :: String -> Bool
check = check' ""
````

(Notice that we don't explicitly mention the input. The way Haskell does
typing and evaluation means that the compiler can just take it as read.)

To help make writing the code a little easier, we'll use a fairly stupid
helper function to tell us which closing bracket to expect given an
opening one. Alas, for the sake of brevity, I've left this as a partial
function: given *some* character from a small set, it will return the
matching bracket. Given a character that isn't a bracket, though, it'll
simply raise an exception and terminate the program.

````{.haskell}
-- | Given an opening bracket, return it's closing counterpart.
expect c = fromJust $ lookup c $ zip "[{(<" "]})>"
````

Finally we get to `check'` -- the function that actually does all of the
work. It takes two parameters -- the stack of closing brackets it should
expect to encounter, and a string to process -- and returns `True` if
the stack and input are both exhausted without encountering an
inconsistency and `False` otherwise.

````{.haskell}
-- | Checks to see if a String contains only correctly nested brackets.
check' :: String -> String -> Bool
````

The first clause in the definition handles the "we've run out of input"
case. If there are no input characters to be processed, then the input
was correct *if and only if* the stack of closing brackets we are
expecting to see is also empty.

````{.haskell}
check' s []    = null s
````

If there is input still to be processed, take the first character `c`.
If `c` is not a bracket, then we continue processing the rest of the
input. If `c` is an opening bracket, then we add its matching closing
bracket to the stack of those we're expecting and continue processing
the rest of the input. If `c` is a closing bracket and we are expecting
to see a closing bracket, then the input was correctly nested if it is
the brackets we were expecting (the top of the stack) and the rest is
nested properly. In any other case, the input must not have been
correctly nested.

````{.haskell}
check' s (c:r)
    | (not$ elem c "[]()<>{}")      = check' s r
    | elem c "([{<"                 = check' (expect c:s) r
    | elem c ")]}>" && (not.null) s = (c == head s) && check' (tail s) r
    | otherwise                     = False
````

There it is: a fairly trivial solution to a fairly trivial problem.
There are definitely better solutions out there (`fold`ing the list into
`Maybe [Char]` where a result of `Just []` is success or maybe
`filter`ing the `iterate`d input) but it works.
