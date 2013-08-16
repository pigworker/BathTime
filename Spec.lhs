Spec
====

> {-# LANGUAGE MultiParamTypeClasses #-}

> module Spec where

> import Tm

> data Spec
>   = CanSpec Integer Can Tele

> data Tele
>   = TNil
>   | TConsIn (Namer, Ty) (Body Tele)

> instance Shuntable Tele where
>   TNil              -^ s  = TNil
>   TConsIn (x, t) b  -^ s  = TConsIn (x, t -^ s) (b -^ s)

> instance Rename Tele where
>   TNil              // r  = TNil
>   TConsIn (x, t) b  // r  = TConsIn (x, t // r) (b // r)

> instance Eval Tele Tele where
>   TNil               -! g = TNil
>   TConsIn (x, t) ts  -! g = TConsIn (x, t -! g) (ts -! g)
