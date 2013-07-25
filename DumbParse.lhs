DumbParse
=========

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

> module DumbParse where

> import Data.Char
> import Data.Monoid
> import Control.Applicative
> import Control.Monad
> import Data.Traversable

> import Tm

> data Globs = Globs
>   {  cans :: [String]
>   }

> newtype Parse x = Parse
>   {parse :: Globs -> [String] -> String -> [(x, String)]}
>   deriving Monoid

> instance Monad Parse where
>   return x = Parse $ \ g l s -> [(x, s)]
>   Parse pa >>= f = Parse $ \ g l s -> do
>     (a, s) <- pa g l s
>     parse (f a) g l s

> instance Applicative Parse where
>   pure = return
>   (<*>) = ap

> instance Functor Parse where
>   fmap = (<*>) . pure

> instance Alternative Parse where
>   empty = mempty
>   (<|>) = mappend

> instance MonadPlus Parse where
>   mzero = empty
>   mplus = (<|>)

> none :: Parse ()
> none = Parse $ \ g l s -> if null s then [((), "")] else []

> one :: (Char -> Bool) -> Parse Char
> one p = Parse $ \ g l s -> case s of
>   c : s | p c  -> [(c, s)]
>   _            -> []

> txt :: String -> Parse ()
> txt s = () <$ traverse (one.(==)) s

> locsp :: Parse [String]
> locsp = Parse $ \ g l s -> [(l, s)]

> bindp :: String -> Parse x -> Parse x
> bindp x p = Parse $ \ g l s -> parse p g (x : l) s

> globsp :: Parse Globs
> globsp = Parse $ \ g l s -> [(g, s)]

> spc :: Parse ()
> spc = () <$ many (one isSpace)

> isIddy :: Char -> Bool
> isIddy c = isAlpha c || elem c "_'"

> identp :: Parse String
> identp = (:) <$> one isAlpha <*> many (one isIddy)

> idNump :: Parse (String, Integer)
> idNump = (,) <$> identp <*>
>            (read <$ txt "^" <*> some (one isDigit) <|> pure 0)

> varLook :: Int -> [String] -> (String, Integer) -> Parse Ne
> varLook i (x : xs)  (y, 0) | x == y  = pure (V i)
> varLook i (x : xs)  (y, j) | x == y  = varLook (i + 1) xs (y, j - 1)
> varLook i (x : xs)  yj               = varLook (i + 1) xs yj
> varLook i []        _                = empty

> varp :: Parse Ne
> varp = join $ varLook 0 <$> locsp <*> idNump

> lamp :: Parse Tm
> lamp = do
>   txt "\\"
>   spc
>   x <- identp
>   spc
>   txt "."
>   spc
>   t <- bindp x bigTm
>   return (L x :. [] :- t)

> bigTm :: Parse Tm
> bigTm = lamp <|> N <$> bigNe <|> weeTm

> weeTm :: Parse Tm
> weeTm 
>   =    id <$ txt "(" <* spc <*> bigTm <* spc <* txt ")"
>   <|>  N <$> weeNe

> bigNe :: Parse Ne
> bigNe = weeNe >>= moreNe

> moreNe :: Ne -> Parse Ne
> moreNe n = (spc *>
>   (    (n :$) <$> weeTm
>   <|>  Car n <$ txt "!"
>   <|>  Cdr n <$ txt "-"
>   ) >>= moreNe) <|> pure n

> weeNe :: Parse Ne
> weeNe = varp

