> {-# LANGUAGE RankNTypes #-}
> module Main where

A Wire was complicated enough but now it's type features six parameters:

< data Wire s e m a b
          ^ ^ ^ ^ ^
  signal --+ | | | |
  event  ----+ | | |
  monad  ------+ | |
  input  --------+ |
  output ----------+

Let's start importing some modules:

> import Prelude hiding ((.))
> import Control.Wire
> import Control.Monad.IO.Class

Let's start with the simplest Wire:

< WConst

This Wire as is easy to guess yield
always the same value.

> stepConstant :: Monad m => Wire s e m a Int
> stepConstant = mkConst . Right $ 42

No matter what we feed to this Wire, the result will
be always the answer to life the universe and everything.

> computeConst :: Monad m
>              => s
>              -> Either e Int
>              -> m (Either e Int, Wire s e m Int Int)
> computeConst = stepWire $ stepConstant

Wire is an instance of Monoid, so Wire can be mappended

> stepSemigroup :: Monad m => Wire s e m a (Sum Int)
> stepSemigroup = mkConst . Right $ Sum 42

> fromRight (Right v) = v
> fromRight _ = undefined

> computeSum :: s -> Either e (Sum Int) -> IO Int
> computeSum s e = do
>   fmap (getSum . fromRight . fst) $
>     stepWire (stepSemigroup <> stepSemigroup) s e

>
> main :: IO ()
> main = do
>   (s, sess') <- stepSession clockSession_
>   let evt = Right 100
>   (res, wire') <- computeConst s evt
>   case res of
>     Left _ -> putStrLn "wire failed"
>     Right v -> print v
>   res <- computeSum s (Right $ Sum 100)
>   print res
