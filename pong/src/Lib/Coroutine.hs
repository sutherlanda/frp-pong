{-# LANGUAGE TupleSections #-}

module Lib.Coroutine
  ( Coroutine,
    Event,
    scan,
    mapE,
    scanE,
    zipE,
    filterE,
    constE,
    mergeE,
    (<++>),
    withPrevious,
    delay,
    integrate,
    derivate,
    watch,
    restartWhen,
    step,
  )
where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.List (foldl', mapAccumL)
import Prelude hiding (id, (.))

newtype Coroutine i o = Coroutine
  { runC :: i -> (o, Coroutine i o)
  }

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
  id = Coroutine (,id)
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

instance ArrowLoop Coroutine where
  loop co =
    Coroutine $ \b ->
      let ((c, d), co') = runC co (b, d)
       in (c, loop co')

scan :: (a -> b -> a) -> a -> Coroutine b a
scan f i = Coroutine $ step i
  where
    step a b =
      let a' = f a b
       in (a', scan f a')

step :: Coroutine i o -> i -> (o, Coroutine i o)
step = runC

-- Events
type Event a = [a]

mapE :: (e -> e') -> Coroutine (Event e) (Event e')
mapE = arr . map

filterE :: (e -> Bool) -> Coroutine (Event e) (Event e)
filterE = arr . filter

constE :: e -> Coroutine (Event e') (Event e)
constE = mapE . const

zipWithC :: (a -> b -> c) -> Coroutine (a, b) c
zipWithC = arr . uncurry

zipE :: Coroutine (Event e, Event e) (Event e)
zipE = zipWithC (++)

scanE :: (a -> e -> a) -> a -> Coroutine (Event e) a
scanE f i = Coroutine $ step i
  where
    step a e =
      let a' = foldl' f a e
       in (a', scanE f a')

mergeE ::
  Coroutine i (Event e) -> Coroutine i (Event e) -> Coroutine i (Event e)
mergeE = liftA2 (++)

(<++>) ::
  Coroutine i (Event e) -> Coroutine i (Event e) -> Coroutine i (Event e)
(<++>) = liftA2 (++)

withPrevious :: a -> Coroutine a (a, a)
withPrevious first = Coroutine $ \i -> ((i, first), step i)
  where
    step old = Coroutine $ \i -> ((i, old), step i)

delay :: a -> Coroutine a a
delay a = withPrevious a >>> arr snd

integrate :: Num a => a -> Coroutine a a
integrate = scan (+)

derivate :: Num a => Coroutine a a
derivate = withPrevious 0 >>> zipWithC (-)

watch :: (a -> Bool) -> Coroutine a (Event a)
watch f =
  Coroutine $ \i ->
    if f i
      then ([i], watch f)
      else ([], watch f)

restartWhen :: Coroutine a b -> Coroutine (a, Event e) b
restartWhen co = Coroutine $ step co
  where
    step c (i, ev) = (o, Coroutine cont)
      where
        (o, c') = runC c i
        cont
          | null ev = step c'
          | otherwise = step co

-- | Testing
intsFrom :: Integer -> Coroutine () Integer
intsFrom n = Coroutine $ const (n, intsFrom (n + 1))

accumSum :: Coroutine Integer Integer
accumSum = scan (+) 0

evalList :: Coroutine i o -> [i] -> [o]
evalList _ [] = []
evalList co (x : xs) = o : evalList co' xs
  where
    (o, co') = runC co x

someFunc :: IO ()
someFunc = do
  let i = intsFrom 5
      sumFrom = intsFrom 0 >>> accumSum
   in do
        print $ evalList i [(), (), ()]
        print $ evalList sumFrom [(), (), (), ()]

someFunc2 :: IO ()
someFunc2 = do
  print $ evalList sumFrom [(), (), ()]
  where
    i = intsFrom 5
    sumFrom = intsFrom 25 >>> accumSum
