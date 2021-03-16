{-# LANGUAGE OverloadedStrings #-}

module Sandbox where

import Control.Monad (unless)
import Control.Monad.Identity hiding (unless)
import Control.Monad.Trans.Class (lift)
import Control.Wire hiding (unless)
import Data.Time
import FRP.Netwire.Move
import Prelude hiding ((.), id)
import SDL hiding (time)

wire :: (Monad m) => Wire s () m a Integer
wire = pure 15

wire2 :: (HasTime t s) => Wire s () m a t
wire2 = time

wire3 :: (HasTime t s, Monad m) => Wire s () m a String
wire3 = for 3 . "yes" <|> "no"

wire4 :: (HasTime t s, Monad m) => Wire s () m a String
wire4 = asSoonAs . at 3 . "blarg"

wire5 :: (HasTime t s, Monad m) => Wire s () m a String
wire5 = asSoonAs . at 2 . "foo" <> asSoonAs . at 3 . "bar"

wire6 :: (HasTime t s, Monad m) => Wire s () m a String
wire6 = for 3 . "yes" --> "no"

wire7 :: (HasTime t s, Monad m) => Wire s () m a t
wire7 = for 3 . time --> time

fun :: (Fractional t, HasTime t s, Monad m) => Wire s () m a String
fun =
  for 2 . "Once upon a time..." --> for 3 .
  "... games were completely imperitive ..." -->
  for 2 .
  "... but then ..." -->
  for 10 .
  ("Netwire 5!" <> anim) -->
  fun
  where
    anim :: (Fractional t, HasTime t s, Monad m) => Wire s () m a String
    anim = holdFor 0.5 . periodic 1 . "Hoo..." <|> "...ray"

challenge :: HasTime t s => Wire s () IO a Double
challenge = integral 0 . pure 0.1

countLoop ::
     Num a
  => Session IO (Timed Int ())
  -> Wire (Timed Int ()) () IO a Int
  -> IO ()
countLoop session wire = do
  (state, nextSession) <- stepSession session
  (Right count, nextWire) <- stepWire wire state (Right 0)
  print count
  countLoop nextSession nextWire

sandbox2 :: IO ()
sandbox2 = countLoop (countSession_ 1) $ time >>> mkSF_ (* 2)

sandbox3 :: IO ()
sandbox3 = testWire clockSession_ wire7

sandbox :: IO ()
sandbox = do
  initializeAll
  window <- createWindow "FRP Pong" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer (countSession_ 1) challenge
  destroyWindow window

appLoop ::
     Renderer
  -> Session IO (Timed Int ())
  -> Wire (Timed Int ()) () IO Double Double
  -> IO ()
appLoop renderer session wire = do
  (state, nextSession) <- stepSession session
  (Right x, nextWire) <- stepWire wire state (Right 0)
  events <- pollEvents
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 255 255 255
  fillRect renderer (Just $ Rectangle (P (V2 (round x) 0)) (V2 10 10))
  present renderer
  unless (qPressed events) $ appLoop renderer nextSession nextWire
  where
    eventIsQPress event =
      case eventPayload event of
        KeyboardEvent keyboardEvent ->
          keyboardEventKeyMotion keyboardEvent == Pressed &&
          keysymKeycode (keyboardEventKeysym keyboardEvent) ==
          KeycodeQ
        _ -> False
    qPressed = any eventIsQPress
