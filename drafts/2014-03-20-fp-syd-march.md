---
title: FP-Syd, March 2014
tags: event, fp-syd, haskell, parallel, accelerated
---

The March 2014 edition of FP-Syd was held on Thursday 20th so that [Yaron
Minsky][1] -- who's in Sydney -- can talk. There were three speakers:

- Robert Clifton-Everest spoke about nested data parallelism with Accelerate.

- Tim

- Yaron

# Rober Clifton-Everest on Accelerate

We have GPUs now which are very powerful.  GEforce titan 2688 cores @
867 MHz. But quite different to CPUs.  Limited instruction set, SIMD.

How to harness? Accelerate is a language embedded in Haskell -- data
parallel language operating over multi dimensional arrays.

````{.haskell}
dotp xs ys = fold (+) 0 (zipWith (*) xs ys)
````

Reusing this definition of `dotp` to implement matrix-vector
multiplication, we might get something like this:

````{.haskell}
mvm m v = generate (index1 (height m))
                   (\i -> the (dotp v (getRow i m))
````

But this results in a rather obtuse error due to nested data
parallelism. This is the most common error encountered by new users of
Accelerate.

Should be something like this:

````{.haskell}
mvm m v = fold (+) 0 (zipWith (*) m (replicateRow (height mat) v))
````

Blelloch and Sabot paper described vectorisation, conver nested
parallelism to flat parallel programme. Programs must be pure; process
is simple but naive; but relies on heavily on compiler
optimisations. There's been lots of work done since 1990 to improve
the quality of the transformed code and optimsied the result. Still
not finished.

## Lifting transformation

foo :: Int -> Float -> Float

Lifting transformation:

L_{n}[[foo]] :: Vector Int -> Vector Float -> Vector Float

L_{n}[[const]] = replicate n const

L_{n}[[var]] = replicate n var -- where x is not lifted

L_{n}[[var]] = var -- where it is lifted

L_{n}[[e1 e2]] = L_{n}[[e1]] L_{n}[[e2]]

L_{n}[[\x.e]] = \x. L_{length x}[[e]]

folds, maps, etc:

L_{n}[[p]] = p^{\uparrow} -- replace a built-in with an equivalent

Example:

bar = \x. 2*x+1

L[[bar]] = \x. (replicate (length x) * 2) *^{} x +^{} (replcate (length x) 1)

## Nesting

Nested vector structures -- pair of vectors:

1. Vector of lengths of the vectors.

2. Vector of all elements smooshed together.

Works for arrays? Yes; don't have to lift to arrays, vectors are enough.

Flatten similarly: store shapes in first vector, values in second.

    ([ (2,3), (1,2) ],[ 1,2,3,4,5,6,1,2 ])

Technique works with progams which *aren't* regular, but it looses the
regularity of, e.g., a two dimensional array. Trying to find the right
approach to detect regular cases and recover the simplest version --
avoiding the lifting, etc.

# Tim Docker - ADL an Actor Definition Language

Haskell's static types a good, but we don't do a very good job of
typing across system boundaries. So we have things like CORBA, thrift,
protocol buffers, etc., etc.

Or we end up with something like a HTTP REST API, with JSON, with --
hopefully -- a good prose description.

Riak -- protocol buffer API. Full of magic values. Client to talk to
this: 2.5kloc of hand-written haskell, 1.6kloc of machine generated.


ADL is an attempt at something better

- A better type system
- A better set of communication primitives
- Abstraction -- remove duplication

Has a curly brace syntax

````
struct Person
{
  String firstName;
  String lastName;
  String age;
  Gender gender;
};

struct Pair<T1, T2> {
  T1 v1;
  T2 v2;
};

union Maybe<T> {
  Void nothing;
  T just;
};

newtype User = Person;
````

Sinks are a communication destination. Represent an endpoint and
encapsulate the parameters for communication: serialisation, protocol,
destination, reliability, "unforgability", etc.

Can be stored, serialised, etc. like any other value, but the
application is required to make the choices about protocol,
serialisation, etc, etc. exactly as expected (no such thing as a
generic protocol).

````
struct Rpc<I,O> {
	I params;
	Sink<O> replyTo;
};

type RpcSvc<I,O> = Sink<Rpc<I,O>>;
````

Using primitives to build up services: key/value stores, streaming
results, continuous queries, etc.

## Custom type mappings

All types have a default value. Can be defined like so:

````
newtype Date = String = "1900-01-01";
````

This is used for serialisation, etc. Can map types to appropriate
native types in generated code.

# Yaron Minsky on 

Chief technologist for Jane Street Capital. Claim to be the biggest
statically type functional shop.

Most of the juice from type systems come from the core elements:
parametric polymorphism, sums, products, etc. A lot of the gee whiz
features are the sort of things you get when you let compiler writers
design your language.

Expected GADTs in Ocaml to be this sort of thing, but totally
surprised by their utility and, especially, their impact on
performance in their systems-oriented code.

Really simple embedding of a language in ML.

````
type value = Int of int | ...
type expr = Value of value | If of expr * expr * expr | ...

val eval : expr -> value
````

When implementing the evaluation function you need to check the types
of values and handle the mismatches. And the API (the sig) is kind of
crappy, allowing you to write nonsense.

So you can try using phantom types to encode these invariants into the
interface:

````
type a' expr

val int_val : int -> int expr
...

val int_eval : int expr -> int
val bool_eval : bool expr -> bool
````

But only slightly change the implementation:

```
type expr_ = (* as before *)

type a' expr = expr_
```

But this leaves a lot of the same correctness burden on the
implementation; and the implementation is still ugly.


## Using GADTs

GADTs extend the ordinary variant structure, allowing the variants to
refine the type of the value.

````
type _ expr =
  | Value : a' value -> a' expr
  | If : bool expr -> a' expr -> a' expr -> a' expr
  | Eq : a' expr -> a' expr -> bool expr
  | ...

let rec eval : type a . a expr -> a = function
  | Value (Int x) -> x
  | Value (Bool x) -> x
  | If (c,t,e) -> if eval c then eval t else eval e
  | ...
````

Not only is it more correct and more beautiful, but also fast. All the
case matches are gone!

## Performance

Only one way:

1. Read message into buffer
2. Look at the message in the buffer
3. Do something
4. Do it again

No parsing, queuing, etc.

So a module which takes a `t` -- a buffer -- with a bunch of accessors
to pull out parts (the transaction id) as immediate values in
registers. But this is restricted to product types. GADTs let you
represent some of this stuff.

## Example

Example wire format

- 0-7 char 'a' or 'b'

- 'a';int;int

- 'b';int;char 'c' or 'd'

- 'c';int

- 'd';int


````
type c_or_d = | C of char.t * float | D of int
type b = int * c_or_d
type a = int * int
type t = ...
````

With a bunch of code written to peek and construct the types from the
buffer.

### Use GADTs

Type hierarchy:

````
msg -> a;
msg -> b -> {c d};
````

````
type 'a t

(* A GADT to get an existential type *)
type packed =
	| T : 'a t -> packed

val of_iobuf : (read_wrtie, Iobuf.seek) Iobuf.t -> packed

(* Uninhabited types to use as labels. We'll use these to refine the
message types, but they have no values. *)
type a
type _ b

type _ typ =
  | A : a type
  | B : _ b type

(* We'll use this to inspect and refine the type of the existential. *)
val typ : 'a t -> 'a typ
````

How does this interface differ in perforamnce? The `_ typ` values fit
in a register.

Then use Ocaml's `Obj.magic` feature (`unsafePerformIO`)

````
let typ t =
  match Iobuf.Peek.char t ~pos:0 with
  | 'a' -> (Obj.magic (A : _ typ) : _ typ)
  | 'b' -> (Obj.magic (B : _ typ) : _ typ)
  |  _  -> failwith "Illegal message"
````

Additional modules for A and B (and then C and D which follow from B
in the same way as A and B from the top).

## Q & A

mirage project?



