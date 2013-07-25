---
title: FP-Syd July 2013
tags: functional programming, fp-syd, haskell, array programming, discipline, typing
location: Sydney, New South Wales
excerpt: 
  Here are some notes from the July, 2013 meeting of the Sydney functional
  programming group. 
---

# Accelerate with foreign functions

More on accelerate. Looking at using it with other GPGPU frameworks. Two
distinct problems:

1. Calling CUDA C programs from Accelerate.

2. Calling Accelerate from CUDA C programs.

## Calling CUDA C from Accelerate

> Smooth Life is Conway's Game of Life generalised to confinuous domains.
>
> Lots of variants (magic numbers). This was smooth life "L".
>
> Gliders can move in any direction.

Relies on FFT as part of in implementation. Write own FFT in Accelerate *or*
use cuFFT library.

The cuFFT library wants pointers to GPU memory as parameters to its function,
but Accelerate is high-level (no pointers into GPU memory). So added a new
operation:

````haskell
foreignAcc :: (Arrays arr, Arrays res, Foreign ff)
           => ff arr res -- ^ The foriegn code
           -> (Acc arr -> Acc res) -- ^ The pure equivalent
           -> Acc res
           -> Acc res
````

Each backend needs to provide its own instance of Foreign; subclass of Typable2
to avoid the expression problem (so that backends can use their own types for
the foreign stuff).

Use an "abstract" monad CIO, which is like IO but has a few new operations:
`allocateArray`, `devicePtrsOfArray`, `peekArray`, `pokeArray`. Here "abstract"
is just "private parts are private".

````haskell
doFFT :: Acc (Array DIM2 Complex)
      -> Acc (Array DIM2 Complex)
doFFT arr = foreignAcc (CuForeign foreignFFT)
                       pureFFT
                       arr
    where
      pureFFT arr = ... a slow, pure Accelerate FFT ...
      foriegnFFT arr = ...
````

You can nest calls to `foreignAcc` in the pure branch to offer implementations
for multiple backends.

## Calling Accelerate programs from C

`foreignAccModule` is a piece of Template Haskell magic which, when a module is
compiled, generates a C header file for the module.

Additional Template Haskell functions `exportAfun1`, `exportAfun2`, etc. are
used to export specific functions (the `dotp` function in this example).

Generates two C functions from `dotp`:

- `dotp_compile(...)` compiles an Accelerate program (the `dotp` program).

- `dotp_run(...)` executes a compiled Accelerate program (the `dotp` program).

# How Mark writes Haskell

Demo of how "I" write Haskell.

> If you use vim you are bad.

Programming has two cultures:

1. Tools-oriented cultures like Java, with lots of IDEs, etc.

2. Language-oriented cultures like Haskell.

ghcmod? is a tool for Haskell; supports emacs *and* vim. Does recompilation and
test on save, integrates with Hoogle (insert module import statements).

Ruby tool called guard. DSL to watch file system changes and do various things
pass code through compiler for fast feedback about compiler errors. Run it in a
window beside emacs and see things happen as you save!

The doctest library allows you to write quickcheck properties in haddock
comments.

Similar: hdevtools for vim. Has a persistent server.

# Ben talking about DDC

> Note to self: the way you've captured the typing rules in this section is
> truly horrible. Please remember LaTeX and figure out how you want to render
> it with Pandoc & Hakyll.

Pushing to make DDC be something like LLVM for functional programming
languages; a generally applicable core language.

## Typing application

$$\large\frac{
	\Gamma \vdash M :: t_{1} \rightarrow t_{2}
	\qquad
	\Gamma \vdash N :: t_{1}
}{
	\Gamma \vdash M N :: t_{2}
}$$

Evaluation

1. M reduces to a value (an abstraction).

2. N reduces to a value

3. Substitution N into M.

4. M[N/x] is new value to reduce.

Is there something in the typing rule which represents all four of these
stages? Not really. (From me: should it be linear logic)

Perhaps adding effects to types.

$$\large\frac{
  \Gamma \vdash M :: t_{1} \rightarrow t_{2} ; e1
  \qquad
  \Gamma \vdash N :: t_{1} ; e2
}{
  \Gamma \vdash M N :: t_{3} ; e_{1} \vee e_{2} \vee e_{3}
}$$

> Where $\vee$ is a lattice join:
>
> $e_{1} \vee e_{2} = e_{2} \vee e_{1}$
> 
> $e_{1} \vee e_{1} = e_{1}$

Now there's something in the typing rule which represents each phase:

1. Is e1
2. Is e2
3. Is e3?
4. Is the join of them?

## Monads

> People who think Haskell is cool might have heard of this thing called a
> monad.

````haskell
return :: a -> m a
bind :: m a -> (a -> m b) -> m b
````

There's no good way to abstract over monads: each `m` must be a single monad.
You can use monad transformers, but they suck.

An effect system allows you to write something like

$$\large foo :: Int \xrightarrow{State \vee IO} Int$$

instead of choosing between options like:

$$\large foo :: Int \rightarrow \text{State Int}$$

$$\large foo :: Int \rightarrow \text{IO Int}$$

$$\large foo :: Int \rightarrow \text{StateT s IO Int}$$

## Impact on kinds

Adding effects forces us to change the kind of the `->`. In Haskell:

````haskell
(->) :: * -> * -> *
````

In an effectful language, every arrow has an effect component:

````haskell
(->) :: * -> * -> Effect -> *
````

Could maybe (and Ben's PhD thesis does) add a `Pure` effect to non-effectful
arrows.

It also changes the nature of application (just substitution) and mixing the
"other stuff" in.

## Value-only languages

What if we remove substitution (can only apply values to values):

$$\large\frac{
  \Gamma \vdash v_{1} :: t_{1} \xrightarrow{e_{1}} t_{2} ; \bot
  \qquad
  \Gamma \vdash v_{2} :: t_{1} ; \bot
}{
  \Gamma \vdash v_{1} v_{2} :: t_{2} ; e_{2}
}$$

(Note, if you haven't already, that an effect of $\bot$ is pure.)

So:

$$\large foo :: Int \rightarrow \text{S (State}\vee\text{IO) Int}$$

Here `S` is a suspended computation which, when invoked, will perform some
actions and return an integer.

Introducing a suspended computation:

$$\large\frac{
  \Gamma \vdash M :: t_{1} ; e_{1}
}{
  \Gamma \vdash suspend M :: S e_{1} t_{1} ; \bot
}$$

Running a suspended computation:

$$\large\frac{
\Gamma \vdash M :: S e_{1} t; e_{2}
}{
\Gamma \vdash run M :: t ; e_{2} \vee e_{1}
}$$


Also need:

$$\large\frac{
\Gamma, x : t_1 \vdash M :: t_2 ; \bot
}{
\Gamma \vdash (\lambda (x:t_1) . M) :: t_1 \rightarrow t_2 ; \bot
}$$

$$\large\frac{
\Gamma \in x:t_1
}{
\Gamma \vdash x :: t_1 ; \bot
}$$

And application 
$$\large\frac{
  \Gamma \vdash M :: t_1 \rightarrow t2; e_1
  \qquad
  \Gamma \vdash N :: t_1 ; e_2
}{
  \Gamma \vdash M N :: t_2 ; e_1 \vee e_2
}$$

Claim that this system is better than the original effect system which forces
us to add an effect to the arrow kind (our arrow has the original pure type)
and also the Haskell approach with monads.

Use `suspend` within a lambda abstraction.

````
\lambda(x:T1). suspend ....
````

It's obvious where to insert `suspend` as abstration bodies must be pure.

`suspend` and `run` are syntactic; generated by compiler for source language.

## Q&A

Constructive logics have judgements like this (M terminates and proves A is
true):

$$\large\Gamma \vdash M :: \text{A true}$$

Also have a judgement like this (*if* M terminates, it proves A):

$$\large\Gamma \vdash M :: \text{A lax}$$

And:

$$\large\frac{
  \Gamma \vdash M :: \text{A lax}
}{
  \Gamma \vdash box M :: \Box \text{A true}
}$$

(See PL summer school video?)

See also the Haskell Symposium paper (papers!) doing extensible effects in
Haskell.

# Yow! Conference

There's a potential for members to get a discounted ticket (group rate). Talk
to Jed about it.
