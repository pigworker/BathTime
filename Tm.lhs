0123456789012345678901234567890123456789012345678901234567890123456789012345678
01234                                                                     45678
01234     Tm                                                              45678
01234                                                                     45678
0123456789012345678901234567890123456789012345678901234567890123456789012345678

There's always a file called Tm, defining the syntax of core terms.

> {-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeSynonymInstances #-}

> module Tm where

> import Data.Monoid

> data Tm
>   = S Sort            -- type of types
>   | Can :@ [Tm]       -- canonical type
>   | Namer :. Body     -- abstraction
>   | Tm :& Tm          -- pairing
>   | Z                 -- zero
>   | C Tm              -- constructor layer
>   | N (Ne Tm)         -- neutral
>   deriving Eq
> infixr  2  :.
> infixr  4  :&
> infix   9  :@

> data Ne t
>   = V Int             -- de Bruijn index
>   | P Decl            -- declared symbol
>   | D Defn Shunt      -- defined symbol, sort-shunted
>   | Ne t :$ t         -- application
>   deriving (Eq, Functor)
> infixl  5  :$

> data Sort
>   = Set Integer       -- predcicative levels
>   | Type              -- one superuniverse
>   | Kind              -- the topsort
>   deriving Eq

> data Can
>   = Pi World          -- function type
>   | Sg World          -- pair type
>   | One               -- unit type
>   deriving Eq

> data Namer
>   = L String
> instance Eq Namer where _ == _ = True

> data Body
>   = K Tm              -- constant
>   | [Tm] :- Tm        -- some lets and a lambda body
> infixr  2  :-
> instance Eq Body where ([] :- b1) == ([] :- b2) = b1 == b2

> data Decl = PName ::: Tm                      -- name and type
>   deriving Eq
> type PName = [(String, Int)]                  -- nice and splittable

> data Defn = DName ::= (Maybe Tm, Tm)          -- could be a hole
>   deriving Eq
> type DName = [(String, Int)]                  -- separate from PName
> type Shunt = Integer                          -- how to shunt sorts, for now

> data World = Dyn | Sta deriving Eq            -- two worlds to start

> class Eval t where
>   (-!) :: t -> [Tm] -> Tm
>   eval :: t -> Tm
>   eval = (-! [])
>   (-^), shunt :: t -> Shunt -> t
>   shunt t s | s == mempty = t
>   shunt t s = t -^ s

> instance Eval Int where
>   (-!) = flip (!!)
>   (-^) = const

> instance Eval Tm where
>   (c :@ ts)       -! g  = c :@ map (-! g) ts
>   (x :. K t)      -! g  = x :. K (t -! g)
>   (x :. h :- b)   -! g  = x :. (map (-! g) h ++ g) :- b
>   (a :& d)        -! g  = (a -! g) :& (d -! g)
>   C t             -! g  = C (t -! g)
>   N n             -! g  = n -! g
>   t               -! _  = t
>   S (Set i)       -^ s  = S (Set (i +^ s))
>   (c :@ ts)       -^ s  = c :@ map (-^ s) ts
>   (x :. K t)      -^ s  = x :. K (t -^ s)
>   (x :. h :- b)   -^ s  = x :. map (-^ s) h :- (b -^ s)
>   (a :& d)        -^ s  = (a -^ s) :& (d -^ s)
>   C t             -^ s  = C (t -^ s)
>   N n             -^ s  = N (n -^ s)
>   t               -^ _  = t

> instance Eval (Ne Tm) where
>   D (_ ::= (Just v, _)) s  -! _  = shunt v s
>   (f :$ a)                 -! g  = (f -! g) $$ (a -! g)
>   n                        -! _  = N n
>   D d s'     -^ s  = D d (mappend s' s)
>   (f :$ a)   -^ s  = (f -^ s) :$ (a -^ s)
>   n          -^ s  = n

> ($$) :: Tm -> Tm -> Tm
> N n            $$ v         = N (n :$ v)
> (_ :. K t)     $$ _         = t
> (_ :. g :- b)  $$ v         = b -! (v : g)
> (a :& _)       $$ Z         = a
> (_ :& d)       $$ (Z :& Z)  = d

> instance Monoid Shunt where
>   mempty   = 0
>   mappend  = (+)

> (+^) :: Integer -> Shunt -> Integer
> (+^) = (+)

