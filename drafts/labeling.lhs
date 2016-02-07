> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE OverlappingInstances #-}

> data Policy label = Policy label
>  deriving (Show, Eq, Ord)

The labels are 

> class Label l

> instance Label ()
> instance Label Int
> instance Label String
> instance (Label x, Label y) => Label (x,y)

So I can have policies which need "no" information to do their thing:

> randomPolicy :: Policy ()
> randomPolicy = Policy ()

> reversePolicy :: Policy ()
> reversePolicy = Policy ()

> oddPolicy :: Policy ()
> oddPolicy = Policy ()

I want to write a combinator which composes simple policies into more complex
policies: 

> combine :: (Label x, Label y) => Policy x -> Policy y -> Policy (x,y)
> combine (Policy m) (Policy n) = Policy (m,n)

This approach works but it keeps all of the information about the policies,
even if the information is just "no information please".

> lolPolicy :: Policy (((),()),())
> lolPolicy = randomPolicy `combine` reversePolicy `combine` oddPolicy

It would be good to, when combining two "no information please" polices to make
the result also a simple "no information please" policy.

````{.haskell}
lolPolicy :: Policy ()
````

I thought that I might be able to do this with a multi-parameter type class and
functional dependencies using something like this:

````{.haskell}
class (Label x, Label y, Label z) => CombinedLabel x y z | x y -> z where
    combineLabels :: x -> y -> z

instance CombinedLabel () () () where
    combineLabels _ _ = ()

instance CombinedLabel x y (x,y) where
    combineLabels = (,)
````

But the compile complains about conflicting functional dependencies.

