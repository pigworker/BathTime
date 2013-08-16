Ty
==

Here's how to check types.

> module Ty where

> import Control.Monad

> import Tm
> import Spec


Choosing Names
--------------

> type Root = ([(String, Int)], Int)

> name :: Root -> Namer -> (PName, Root)
> name (sis, i) (L s) = (sis ++ [(s, i)], (sis, i+1))


A Checking Monad
----------------

> newtype Chk x = Chk {chk :: TCInfo -> World -> Maybe x}

> data TCInfo = TCInfo
>   {  rooty  :: Root
>   ,  canty  :: [(Can, (Integer, Tele))]
>   }

> instance Monad Chk where
>   return x = Chk $ \ r w -> Just x
>   Chk ac >>= k = Chk $ \ r w -> do
>     a <- ac r w
>     chk (k a) r w

> instance MonadPlus Chk where
>   mzero = Chk $ \ _ _ -> mzero
>   mplus ac bc = Chk $ \ r w -> mplus (chk ac r w) (chk bc r w)

We can bind a name for a problem. Beware of the scope of the solution.

> (!-) ::  (World, Namer, Tm) -> (Ne -> Chk t) -> Chk t
> (w, x, a) !- j = Chk $ \ info w -> do
>   let (x', r') = name (rooty info) x
>   let n = P ((w, x') ::: a)
>   t' <- chk (j n) (info {rooty = r'}) w
>   return t'

We can check that we can be accessed from a given world.

> seeFrom :: World -> Chk ()
> seeFrom w = Chk $ \ _ w' -> guard (w %> w')

We can move to an appropriate new world to check an argument

> arg :: World -> Chk t -> Chk t
> arg w' c = Chk $ \ r w -> chk c r (w' $% w)

We can shift to a root with a zero counter

> root :: String -> Chk t -> Chk t
> root x c = Chk $ \ info w ->
>   let  (sis, j) = rooty info
>   in   chk c (info {rooty = (sis ++ [(x, j)], 0)}) w

We can turn a name into an index if it matches the root.

> inx :: Decl -> Chk Ne
> inx d@((_, x) ::: _) = Chk $ \ info _ ->
>   let (y, l) = rooty info
>       chomp [] [(_, k)] = V (l - k - 1)
>       chomp (x : xs) (y : ys) | x == y = chomp xs ys
>       chomp _ _ = P d
>   in  return (chomp x y)

We can check for being canonical.

> canChk :: Can -> Chk (Integer, Tele)
> canChk c = Chk $ \ info w -> lookup c (canty info)


Typechecking Terms
------------------

We push types into `Tm`s...

> tyChk :: Ty -> Tm -> Chk ()

...but when they're embedded `Ne`s, we synthesize a type, then check
that what we get is a subtype of what we want. Subtyping is just
contra-and-covariant cumulativity.

> tyChk ty (N n) = do
>   ty' <- tySyn n
>   subChk ty' ty

We allow type formation only in the `Sta` world. Those things should not
be around at run time.

> tyChk (S i) tm = do
>   seeFrom Sta
>   case tm of

Let's be careful about universe levels, for a change.

>     S j -> guard (i > j)

Let's close all sorts under function spaces. Choosing a *binding* presentation
of the function type lets us close `Kind` under function spaces without
using `Kind` as a subcomponent of a type.

>     x@(_, _, a) :-> b -> do
>       tyChk (S i) a
>       x !- \ x -> tyChk (S i) (b -/ x)

For the canonical sets, we must look them up, to find their minimum level
and their argument telescope. We must be forming a set at at least the
minimum level, and we shunt the telescope up by the difference. That is,
every level above the minimum is closed under the construction.

>     c :@ ss -> do
>       (j, ts) <- canChk c
>       case i of
>         Set i | i >= j -> teleChk (shunt ts (i - j)) ss
>         _ -> mzero

>     _ -> mzero

To check a function, go under the binder.

> tyChk ((w, x, a) :-> b) (y :. t) =
>   (w, y, a) !- \ y -> tyChk (b -$ N y) (t -/ y)

Nothing else is implemented yet.

> tyChk _ _ = mzero

For `Ne`s, we expect to synthesize types.

> tySyn :: Ne -> Chk Ty

Parameters should be in a world from which ours is accessible.

> tySyn (P ((w, _) ::: a)) = do
>   seeFrom w
>   return a

Ditto definitions. We must `shunt` the type of the definition in
accordance with its usage.

> tySyn (D ((w, _) ::= (_, a)) s) = do
>   seeFrom w
>   return (shunt a s)

To synthesize the type of an application, synthesize the type
of the function, then check the argument in the world that's
appropriate to the function type, given the world where it's
being used.

> tySyn (f :$ a) = do
>   (w, _, d) :-> t <- tySyn f
>   arg w $ tyChk d a
>   return (t -$ eval a)


Subtyping
---------

Subtyping is cumulativity for sorts, contra-and-covariant for functions,
and otherwise degenerates to definitional equality.

> subChk :: Ty -> Ty -> Chk ()
> subChk  (S i)                 (S j)                    = guard (i <= j)
> subChk  ((w1, _, a1) :-> b1)  (x2@(w2, _, a2) :-> b2)  = do
>   guard (w1 == w2)
>   subChk a2 a1
>   x2 !- \ x -> subChk (b1 -$ N x) (b2 -$ N x)
> subChk  s                     t                        = eqChk (S Kind) (s, t)


Definitional Equality
---------------------

Definitional equality is, for now, syntactic equality of eta-expanded values.

> eqChk :: Ty -> (Tm, Tm) -> Chk ()
> eqChk t (a, b) = do
>   a' <- root "Q" (quote t a)
>   b' <- root "Q" (quote t b)
>   guard (a == b)


Quotation
---------

We standardize values with respect to their types.

> quote :: Ty -> Tm -> Chk Tm

Always start with the types that allow eta-expansion.

> quote ((w, x, a) :-> b) f = do
>   let z = case f of
>         x :. _  -> x
>         _       -> x
>   t' <- (w, z, a) !- \ z -> quote (b -$ N z) (f $$ N z)
>   return (z :. [] :- t')

Then add structural rules for canonical things.

> quote _ (S i) = return (S i)

For neutrals, perform type reconstruction, then throw the type away.

> quote _ (N n) = do
>   (n', _) <- quone n
>   return (N n')

Neutrals should tell us what type they are, so we can standardize the
way they are used.

> quone :: Ne -> Chk (Ne, Ty)

For parameters, check whether it's really a local variable introduced
since the quotation root: if so, convert it to de Bruijn.

> quone (P d@(_ ::: a)) = do
>   x <- inx d
>   return (x, a)

For applications, quote the function, recovering its type. That shows
how to quote the argument.

> quone (f :$ a) = do
>   (f', (_, _, d) :-> t) <- quone f
>   a' <- quote d a
>   return (f' :$ a', t -$ a)

Note that, in the above `t -$ a`, it has to be `a`, which has the right
scope, rather than `a'` which is *not* a value and may contain dangling
de Bruijn indices.


Checking a List Fits a Telescope
--------------------------------

> teleChk :: Tele -> [Tm] -> Chk ()
> teleChk TNil                []        = return ()
> teleChk (TConsIn (x, t) b)  (s : ss)  = do
>   tyChk t s
>   teleChk (b -$ eval s) ss
> teleChk _ _ = mzero
