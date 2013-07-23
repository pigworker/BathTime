Tm
==

> {-# LANGUAGE TypeSynonymInstances #-}

(So far, -XKitchenSink is to be resisted.)

> module Tm where

There's always a file called Tm, defining the syntax of core terms.

> import Data.Monoid


Abstract Syntax
---------------

`Tm` is the type of *&beta;-normal* terms, defined mutually with the neutrals,
`Ne`.

> data Tm
>   -- making types
>   = S Sort                            -- type of types
>   | (World, Namer, Tm) :-> Body       -- function types
>   | Can :@ [Tm]                       -- other canonical type
>   -- making values
>   | Namer :. Body                     -- abstraction
>   | Tm :& Tm                          -- pairing
>   | Z                                 -- zero
>   | C Tm                              -- constructor for isorecursion
>   -- embedding stuck open terms
>   | N Ne                              -- neutral
>   deriving Eq
> infixr  2  :.
> infixr  4  :&
> infix   9  :@

> data Ne
>   -- head forms
>   = V Int             -- de Bruijn index
>   | P Decl            -- declared symbol
>   | D Defn Shunt      -- defined symbol, sort-shunted
>   -- elimination forms
>   | Ne :$ Tm          -- application
>   | Car Ne            -- first projection
>   | Cdr Ne            -- second projection
>   deriving Eq
> infixl  5  :$


Sorts
-----

`Sort` defines a cumulative predicative hierarchy of `Set` universe,
with a `Type` of all `Set`s and a `Kind` of all `Type`s. The idea is
that large eliminators will have motives over `Type`, and that their
types will be well `Kind`ed. `Kind` is a topsort, never used as a term.

> data Sort
>   = Set Integer       -- predcicative levels
>   | Type              -- one superuniverse
>   | Kind              -- the topsort
>   deriving (Eq, Ord)


Canonical Types
---------------

The following is bound to change, but for now, let us just use strings as
type constructors.

> type Can = String -- ORLY?


Worlds
------

The `World` in a function type says "where" the argument will come from.

> data World = Dyn | Sta deriving Eq            -- two worlds to start

In the first instance, we have a `Dyn`amic world for run time stuff and a
`Sta`tic world for stuff which will be erased before run time. More interesting
arrangements are entirely possible.

`World`s are preordered by accessibility. Here we say exactly that `Sta`tic
stuff may not be used in places which survive to run time.

> (%>) :: World -> World -> Bool     -- accessibility
> Sta  %>  Dyn  = False
> _    %>  _    = True

`World`s are slightly polymorphic, in that the `World` where a
function is being *applied* can affect where its argument should come
from. Here, we have that any `Sta`tic usage may take `Sta`tic arguments.

> ($%) :: World{-fun domain-} -> World{-application-} -> World{-argument-}
> _  $%  Sta  = Sta
> w  $%  _    = w


Binding Variables
-----------------

The name on a binder is really just advice for how to pick a nice name for
the variable when a term is being displayed. It's never important for
equality testing.

> data Namer
>   = L String
> instance Eq Namer where _ == _ = True

The representation of bodies binding a variable has a handy special case
for vacuous binding.

> data Body
>   = K Tm              -- constant result for vacuous binding
>   | [Tm] :- Tm        -- some lets and a lambda body
> infixr  2  :-
> instance Eq Body where ([] :- t1) == ([] :- t2) = t1 == t2

Meanwhile, the `g :- t` takes an environment `g` and a body `t` where
`t` has `length g + 1` more de Bruijn indices available, so that 0 refers
to the abstracted variable and 1..n refer into `g`. This is exactly the
structure you need in order to suspend evaluation at a lambda.

When we standardize bodies for equality testing, we don't use the vacuous
representation, and we compute out all the way under binders, so we don't
cache environments. Correspondingly, if we ever test bodies other than
`[] :- t` for equality, then something is amiss.


Free Names
----------

These always carry full contextual information, which is shared.

Declarations attach a type to a name.

> data Decl = PName ::: Tm                      -- name and type
>   deriving Eq
> type PName = [(String, Int)]                  -- nice and splittable

Global definitions attach a type to a name, and if they're finished,
a value. Definitions may be invoked with a shunt to their set levels.

> data Defn = DName ::= (Maybe Tm, Tm)          -- could be a hole
>   deriving Eq
> type DName = [(String, Int)]                  -- separate from PName


Shunting Set Levels
-------------------

A `Shunt` describes a strictly monotonic function on natural numbers.
Here, just adding a constant (but we could work harder and get them all).

> type Shunt = Integer                          -- how to shunt sorts, for now

They should be a monoid...

> instance Monoid Shunt where
>   mempty   = 0
>   mappend  = (+)

...with an action on natural numbers.

> (+^) :: Integer -> Shunt -> Integer
> (+^) = (+)

We shall ensure that typing respects shunting, so definitions (which
are closed and global) can always be shunted.


Evaluation
----------

Things you can evaluate are things which give you a term if you give
them an environment for their dangling de Bruijn indices. Locally, an
environment is a list with the value for `V 0` at its head.  Anything
you can evaluate must also be amenable to a shunt of its set levels.

> class Eval t where
>   (-!) :: t -> [Tm] -> Tm
>   eval :: t -> Tm
>   eval = (-! [])
>   (-^), shunt :: t -> Shunt -> t
>   shunt t s | s == mempty = t
>   shunt t s = t -^ s

Of course, we can evaluate terms in a tediously structural manner.

> instance Eval Tm where -- nothing exciting happens
>
>   (c :@ ts)          -! g  = c :@ map (-! g) ts
>   ((w, x, a) :-> b)  -! g  = (w, x, a -! g) :-> evalBody b g
>   (x :. b)           -! g  = x :. evalBody b g
>   (a :& d)           -! g  = (a -! g) :& (d -! g)
>   C t                -! g  = C (t -! g)
>   N n                -! g  = n -! g
>   t                  -! _  = t
>
>   S (Set i)          -^ s  = S (Set (i +^ s))
>   ((w, x, a) :-> b)  -^ s  = (w, x, a -^ s) :-> shuntBody b s
>   (c :@ ts)          -^ s  = c :@ map (-^ s) ts
>   (x :. b)           -^ s  = x :. shuntBody b s
>   (a :& d)           -^ s  = (a -^ s) :& (d -^ s)
>   C t                -^ s  = C (t -^ s)
>   N n                -^ s  = N (n -^ s)
>   t                  -^ _  = t

Here's where the environment gets stuck. In the h :- t case, h scopes
over g, so we evaluate it to get a closed environment for the inner
indices, then we append g, which gives closed values for the outer
environment. (This could be optimized if we insisted that every term in `h`
must be closed.)

> evalBody :: Body -> [Tm] -> Body
> evalBody (K t)     g  = K (t -! g)
> evalBody (h :- t)  g  = (map (-! g) h ++ g) :- t

> shuntBody :: Body -> Shunt -> Body
> shuntBody (K t)     s  = K (t -^ s)
> shuntBody (h :- t)  s  = map (-^ s) h :- t

It's the neutral terms which unstick when you give values to variables,
so here, variables get looked up and definitions expanded, getting us
to beta-delta-weak-head-normal form. Elimination forms map to their...

> instance Eval Ne where -- all the action is
>
>   V i                      -! g  = g !! i
>   D (_ ::= (Just v, _)) s  -! _  = shunt v s
>   (f :$ a)                 -! g  = (f -! g) $$ (a -! g)
>   Car p                    -! g  = car (p -! g)
>   Cdr p                    -! g  = cdr (p -! g)
>   n                        -! _  = N n
>
>   D d s'     -^ s  = D d (mappend s' s)
>   (f :$ a)   -^ s  = (f -^ s) :$ (a -^ s)
>   Car n      -^ s  = Car (n -^ s)
>   Cdr n      -^ s  = Cdr (n -^ s)
>   n          -^ s  = n

...excitingly computational counterparts, which get stuck if you
feed them neutral things to eliminate, but really do something if
you feed them canonical values.

> ($$) :: Tm -> Tm -> Tm
> N n            $$ v         = N (n :$ v)
> (_ :. K t)     $$ _         = t
> (_ :. g :- b)  $$ v         = b -! (v : g)

> car :: Tm -> Tm
> car (N n)     = N (Car n)
> car (a :& _)  = a

> cdr :: Tm -> Tm
> cdr (N n)     = N (Car n)
> cdr (_ :& d)  = d


Renaming
--------

We need to be able to bind the outermost de Bruijn index to a name (or
indeed any closed neutral term).

> class Rename t where
>   (//) :: t -> (Int, Ne) -> t

> instance Rename Ne where -- the renaming might happen
>   V i       // (j, n) | i == j  = n
>   (f :$ a)  // r                = (f // r) :$ (a // r)
>   Car n     // r                = Car (n // r)
>   Cdr n     // r                = Cdr (n // r)
>   n         // _                = n

> instance Rename Body where -- the renaming might shift
>    K t       // r         = K (t // r)
>    (h :- t)  // r@(j, n)  = map (// r) h :- (t // (length h + 1 + j, n))

> instance Rename Tm where -- all is structural
>   ((w, x, a) :-> b)  // r  = (w, x, a // r) :-> b // r
>   (x :. b)           // r  = x :. (b // r)
>   (a :& d)           // r  = (a // r) :& (d // r)
>   C t                // r  = C (t // r)
>   N n                // r  = N (n // r)
>   t                  // _  = t

