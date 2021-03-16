{-# LANGUAGE TupleSections #-}

module Lib
  ( someFunc
  ) where

import Control.Arrow
import Control.Category
import Data.List (mapAccumL)
import Prelude hiding ((.), id)

newtype Coroutine i o =
  Coroutine
    { runC :: i -> (o, Coroutine i o)
    }

intsFrom :: Integer -> Coroutine () Integer
intsFrom n = Coroutine $ const (n, intsFrom (n + 1))

instance Functor (Coroutine i) where
  fmap f co =
    Coroutine $ \i ->
      let (o, co') = runC co i
       in (f o, fmap f co')

instance Applicative (Coroutine i) where
  pure x = Coroutine $ const (x, pure x)
  cof <*> cox =
    Coroutine $ \i ->
      let (f, cof') = runC cof i
          (x, cox') = runC cox i
       in (f x, cof' <*> cox')

instance Category Coroutine where
  id = Coroutine (, id)
  cof . cog =
    Coroutine $ \i ->
      let (x, cog') = runC cog i
          (y, cof') = runC cof x
       in (y, cof' . cog')

instance Arrow Coroutine where
  arr f = Coroutine $ \i -> (f i, arr f)
  first co =
    Coroutine $ \(a, b) ->
      let (c, co') = runC co a
       in ((c, b), first co')

evalList :: Coroutine i o -> [i] -> [o]
evalList _ [] = []
evalList co (x:xs) = o : evalList co' xs
  where
    (o, co') = runC co x

scan :: (a -> b -> a) -> a -> Coroutine b a
scan f i = Coroutine $ step i
  where
    step a b =
      let a' = f a b
       in (a', scan f a')

accumSum :: Coroutine Integer Integer
accumSum = scan (+) 0

-- | Main
someFunc :: IO ()
someFunc = do
  let i = intsFrom 5
      sumFrom = intsFrom 0 >>> accumSum
   in do print $ evalList i [(), (), ()]
         print $ evalList sumFrom [(), (), (), ()]
