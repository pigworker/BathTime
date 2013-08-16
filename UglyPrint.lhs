UglyPrint
=========

> module UglyPrint where

> import Tm
> import Spec

> bigTm :: [String] -> Tm -> String
> bigTm noms (N n)  = bigNe noms n
> bigTm noms ((w, x, s) :-> K t) =
>   blah noms s ++ " -" ++ worldAnno w ++ "> " ++ bigTm noms t
>   where blah = case t of
>           N _  -> bigTm
>           C _  -> bigTm
>           _    -> weeTm
> bigTm noms ((w, L x, s) :-> [] :- t) =
>   "(" ++ x ++ " :" ++ worldAnno w ++ " " ++ bigTm noms s ++ ") -> " ++
>   bigTm (x : noms) t
> bigTm noms (c :@ ts) = c ++ (ts >>= \ t -> " " ++ weeTm noms t)
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
> dataTms noms t         = ", " ++ bigTm noms t

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
>     fetch i (x : xs)  = case fetch (i - 1) xs of
>       (y, j)  | x == y     -> (y, j + 1)
>               | otherwise  -> (y, j)
> weeNe noms (P ((_, p) ::: _)) = show p
> weeNe noms (D ((_, d) ::= _) 0) = show d
> weeNe noms (D ((_, d) ::= _) s) = show d ++ "^" ++ show s
> weeNe noms t = "(" ++ bigNe noms t ++ ")"

> worldAnno :: World -> String
> worldAnno Dyn = ""
> worldAnno Sta = "*"

> instance Show Sort where
>   show (Set 0)  = "Set"
>   show (Set i)  = "Set^" ++ show i
>   show Type     = "Type"
>   show Kind     = "Kind"

> tele :: [String] -> Tele -> String
> tele noms TNil = ""
> tele noms (TConsIn (L x, t) ts) =
>     "(" ++ x ++ " : " ++ bigTm noms t ++ ")" ++ mo ts where
>   mo (K ts) = tele noms ts
>   mo ([] :- ts) = tele (x : noms) ts

> spec :: Spec -> String
> spec (CanSpec i c ts) = show (Set i) ++ " :> " ++ c ++ case ts of
>   TNil -> ""
>   _ -> " " ++ tele [] ts
