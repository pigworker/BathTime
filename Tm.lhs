0123456789012345678901234567890123456789012345678901234567890123456789012345678
01234                                                                     45678
01234     Tm                                                              45678
01234                                                                     45678
0123456789012345678901234567890123456789012345678901234567890123456789012345678

There's always a file called Tm, defining the syntax of core terms.

> {-# LANGUAGE DeriveFunctor #-}

> module Tm where

> data Tm
>   = S Sort            -- type of types
>   | Can :@ [Tm]       -- canonical type
>   | Namer :. Body     -- abstraction
>   | Tm :& Tm          -- pairing
>   | Z                 -- zero
>   | C Tm              -- constructor layer
>   | N (Ne Tm)         -- neutral
>   deriving Eq

> data Ne t
>   = V Int             -- de Bruijn index
>   | P Decl            -- declared symbol
>   | D Defn Shift      -- defined symbol, sort-shifted
>   | Ne t :$ t         -- application
>   deriving (Eq, Functor)

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
>   | Tm :-! [Tm]       -- term with n+1 free vars, n values
> instance Eq Body where (b1 :-! []) == (b2 :-! []) = b1 == b2

> data Decl = PName ::: Tm                      -- name and type
>   deriving Eq
> type PName = [(String, Int)]                  -- nice and splittable

> data Defn = DName ::= (Maybe Tm, Tm)          -- could be a hole
>   deriving Eq
> type DName = [(String, Int)]                  -- separate from PNamr
> type Shift = Integer                          -- how to shift sorts

> data World = Dyn | Sta deriving Eq            -- two worlds to start
