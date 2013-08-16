SynTest
=======

> module SynTest where

> import Control.Applicative

> import DumbParse
> import UglyPrint

> myGlobs :: Globs
> myGlobs = Globs { cans =  [] }

> tripTm :: String -> String
> tripTm s = case parse (bigTmp <* none) myGlobs [] s of
>   [(t, _)] -> bigTm [] t

> tmSamples :: [(String, String)]
> tmSamples = map (\ s -> (s, tripTm s))
>   [  "Set"
>   ,  "Set^1"
>   ,  "Type"
>   ,  "Kind"
>   ,  "(X :* Type) -> X -> X"
>   ,  "(A :* Type) -> (B :* A -> Type) -> (C : (a : A) -> B a -> Type) -> ((a : A) -> (b : B a) -> C a b) -> (g : (a : A) -> B a) -> (a : A) -> C a (g a)"
>   ]

> tripSpec :: String -> String
> tripSpec s = case parse (specp <* none) myGlobs [] s of
>   [(t, _)] -> spec t
