UglyPrint
=========

> module UglyPrint where

> import Tm

> bigTm :: [String] -> Tm -> String
> bigTm noms (N n)  = bigNe noms n
> bigTm noms ((w, x, s) :-> K t) =
>   weeTm noms s ++ worldArrow w ++ bigTm noms t
> bigTm noms ((w, L x, s) :-> [] :- t) =
>   "(" ++ x ++ " : " ++ bigTm noms s ++ ")" ++ worldArrow w ++
>   bigTm (x : noms) t
> bigTm noms (L x :. K t) = "\\ _ . " ++ bigTm noms t
> bigTm noms (L x :. [] :- t) =
>   "\\ " ++ x ++ " . " ++ bigTm (x : noms) t
> bigTm noms (s :& t) = weeTm noms s ++ " / " ++ bigTm noms t
> bigTm noms t      = weeTm noms t

> weeTm :: [String] -> Tm -> String
> weeTm noms (N n)  = weeNe noms n
> weeTm noms (S i)  = show i
> weeTm noms Z      = "0"
> weeTm noms (C t)  = "[" ++ dataTms noms t ++ "]"
> weeTm noms t      = "(" ++ bigTm noms t ++ ")"

> dataTms :: [String] -> Tm -> String
> dataTms noms Z         = ""
> dataTms noms (a :& Z)  = weeTm noms a
> dataTms noms (a :& d)  = weeTm noms a ++ " " ++ dataTms noms d
> dataTms noms t         = "| " ++ bigTm noms t

> bigNe :: [String] -> Ne -> String
> bigNe noms (f :$ a) = bigNe noms f ++ " " ++ weeTm noms a
> bigNe noms (Car p) = bigNe noms p ++ " !"
> bigNe noms (Cdr p) = bigNe noms p ++ " -"
> bigNe noms t = weeNe noms t

> weeNe :: [String] -> Ne -> String
> weeNe noms (V i) = case fetch i noms of
>   (s, 0) -> s
>   (s, i) -> s ++ "^" ++ show i
>   where
>     fetch 0 (x : _)   = (x, 0)
>     fetch i (x : xs)  = case fetch i xs of
>       (y, j)  | x == y     -> (y, j + 1)
>               | otherwise  -> (y, j)
> weeNe noms (P ((_, p) ::: _)) = show p
> weeNe noms (D ((_, d) ::= _) 0) = show d
> weeNe noms (D ((_, d) ::= _) s) = show d ++ "^" ++ show s
> weeNe noms t = "(" ++ bigNe noms t ++ ")"

> worldArrow :: World -> String
> worldArrow Dyn = " -> "
> worldArrow Sta = " => "

> instance Show Sort where
>   show (Set 0)  = "Set"
>   show (Set i)  = "Set^" ++ show i
>   show Type     = "Type"
>   show Kind     = "Kind"
