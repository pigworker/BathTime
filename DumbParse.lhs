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

> keywords :: [String]
> keywords = ["Set", "Type", "Kind"]

> txt :: String -> Parse ()
> txt s = () <$ traverse (one.(==)) s

> locsp :: Parse [String]
> locsp = Parse $ \ g l s -> [(l, s)]

> bindp :: String -> Parse x -> Parse x
> bindp x p = Parse $ \ g l s -> parse p g (x : l) s

> globsp :: Parse Globs
> globsp = Parse $ \ g l s -> [(g, s)]

> notp :: Parse x -> Parse ()
> notp (Parse f) = Parse $ \ g l s ->
>   if null (f g l s) then [((), s)] else []

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
> varp = do
>   cs <- cans <$> globsp
>   ls <- locsp
>   yj@(x, _) <- idNump
>   guard (not (elem x cs))
>   guard (not (elem x keywords))
>   varLook 0 ls yj

> lamp :: Parse Tm
> lamp = do
>   txt "\\" ;  spc
>   x <- identp
>   spc ; txt "." ; spc
>   t <- bindp x bigTmp
>   return (L x :. [] :- t)

> canp :: Parse Tm
> canp = do
>   cs <- cans <$> globsp
>   c <- identp
>   guard (elem c cs)
>   ts <- many (spc *> weeTmp)
>   return (c :@ ts)

> arrp :: Parse Tm
> arrp = do
>   txt "(" ; spc
>   x <- identp
>   spc ; txt ":" ; spc
>   s <- bigTmp
>   spc ; txt ")" ; spc
>   w <- warrowp
>   spc
>   t <- bindp x bigTmp
>   return ((w, L x, s) :-> [] :- t)

> sortp :: Parse Sort
> sortp
>   =    Set <$ txt "Set" <*>
>          (read <$ txt "^" <*> some (one isDigit) <|> pure 0)
>   <|>  Type <$ txt "Type"
>   <|>  Kind <$ txt "Kind"

> bigTmp :: Parse Tm
> bigTmp
>   =    lamp <|> arrp
>   <|>  (S <$> sortp <|> canp <|> N <$> bigNep <|> weeNoNep) >>= moreTmp

> weeNoNep :: Parse Tm
> weeNoNep 
>   =    id <$ txt "(" <* spc <*> bigTmp <* spc <* txt ")"
>   <|>  Z <$ txt "(" <* spc <* txt ")"
>   <|>  C <$ txt "[" <* spc <*> dataTmp <* spc <* txt "]"


> weeTmp :: Parse Tm
> weeTmp
>   =    id <$> weeNoNep
>   <|>  N <$> weeNep

> moreTmp :: Tm -> Parse Tm
> moreTmp s
>   =    (s :&) <$ spc <* txt "," <* spc <*> bigTmp
>   <|>  vaca s <$ spc <*> warrowp <* spc <*> bigTmp
>   <|>  s <$ notp (spc *> warrowp)
>   where
>     vaca s w t = (w, L "_", s) :-> K t

> warrowp :: Parse World
> warrowp = Sta <$ txt "=>" <|> Dyn <$ txt "->"

> dataTmp :: Parse Tm
> dataTmp 
>   =   (:&) <$> weeTmp <* spc <*> dataTmp
>   <|> id <$ txt "," <* spc <*> bigTmp
>   <|> pure Z

> bigNep :: Parse Ne
> bigNep = weeNep >>= moreNep

> moreNep :: Ne -> Parse Ne
> moreNep n = (spc *>
>   (    (n :$) <$> weeTmp
>   <|>  Car n <$ txt "!"
>   <|>  Cdr n <$ txt "-"
>   ) >>= moreNep) <|> pure n

> weeNep :: Parse Ne
> weeNep = varp

