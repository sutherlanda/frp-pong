{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Pong where

import Control.Arrow
import Control.Concurrent
import Data.Bifunctor (bimap)
import qualified Data.Set as S
import Data.Time.Clock.POSIX
import Data.Word
import Lib.Coroutine
import qualified SDL
import SDL (($=))

type Keyboard = S.Set SDL.Keysym

type Pos = (Int, Int)

type PlayerPos = Pos

type Size = (Int, Int)

type Rect = (Pos, Size)

type Rects = [Rect]

type BallPos = Pos

type Velocity = (Int, Int)

ballInitPos = (400, 200)

ballSize = (8, 8)

ballInitVel = (-6, -6)

topWall = 10

bottomWall = 590

batSpeed = 5

batSize = (10, 40)

startPos = 200

data BallBounce
  = HBounce
  | VBounce

vecMul :: Int -> (Int, Int) -> (Int, Int)
vecMul c (x, y) = (x * c, y * c)

vecAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
vecAdd (a, b) (c, d) = (a + c, b + d)

bounce :: Velocity -> BallBounce -> Velocity
bounce (dx, dy) b =
  case b of
    HBounce -> (-dx, dy)
    VBounce -> (dx, -dy)

wallBounce :: Coroutine BallPos (Event BallBounce)
wallBounce =
  proc blPos ->
  do collisionEvt <- watch
                       (\ (_, y) -> y < topWall || y > bottomWall)
                       -< blPos
     constE VBounce -< collisionEvt

batBounce :: Coroutine (PlayerPos, BallPos) (Event BallBounce)
batBounce =
  proc (plPos, blPos) ->
  do collisionEvt <- watch collision -< (plPos, blPos)
     constE HBounce -< collisionEvt

collision :: (PlayerPos, BallPos) -> Bool
collision ((px, py), (bx, by)) = abs (px - bx) < w' && abs (py - by) < h'
  where
    w' = (bw + pw) `div` 2
    h' = (bh + ph) `div` 2
    (bw, bh) = ballSize
    (pw, ph) = batSize

mkRect :: Pos -> Size -> Rect
mkRect (x, y) (w, h) = ((x - w', y - h'), (w, h))
  where
    w' = w `div` 2
    h' = h `div` 2

game :: Coroutine Keyboard [Rect]
game =
  proc kb ->
  do plPos <- playerPos -< kb
     blPos <- resettingBallPos -< plPos
     returnA -< [mkRect plPos batSize, mkRect blPos ballSize]

playerPos :: Coroutine Keyboard PlayerPos
playerPos =
  proc kb ->
  do spd <- playerSpeed -< kb
     y <- integrate startPos -< spd
     returnA -< (10, y)

playerSpeed :: Coroutine Keyboard Int
playerSpeed = arr keyboardDir
  where
    keyboardDir kb
      | keyDown SDL.KeycodeUp kb = -batSpeed
      | keyDown SDL.KeycodeDown kb = batSpeed
      | otherwise = 0

ballPos :: Coroutine PlayerPos BallPos
ballPos =
  proc plPos ->
  do rec bounces <- (batBounce -< (plPos, pos)) <++>
                      (wallBounce -< pos)
         vel <- scanE bounce ballInitVel -< bounces
         pos <- delay ballInitPos <<< scan vecAdd ballInitPos -< vel
     returnA -< pos

resettingBallPos :: Coroutine PlayerPos BallPos
resettingBallPos =
  proc plPos ->
  do rec pos <- restartWhen ballPos -< (plPos, reset)
         reset <- watch outOfBounds -< pos
     returnA -< pos
  where
    outOfBounds (x, _) = x < 0 || x > 800

run :: IO ()
run = do
  SDL.initializeAll
  window <- SDL.createWindow "FRP Pong" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  appLoop renderer S.empty [] game
  SDL.destroyWindow window

appLoop ::
     SDL.Renderer -> Keyboard -> Rects -> Coroutine Keyboard Rects -> IO ()
appLoop renderer kb prevRects gameLogic = do
  kb' <- parseEvents kb
  let (newRects, gameLogic') = step gameLogic kb'
  draw renderer newRects
  SDL.delay 10
  appLoop renderer kb' newRects gameLogic'

draw :: SDL.Renderer -> Rects -> IO ()
draw renderer rects = do
  SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
  SDL.clear renderer
  SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 255
  mapM_ (drawRect renderer) rects
  SDL.present renderer

drawRect :: SDL.Renderer -> Rect -> IO ()
drawRect renderer (rPos, rSize) = do
  let (x, y) = bimap fromIntegral fromIntegral rPos
  let (w, h) = bimap fromIntegral fromIntegral rSize
  SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)

parseEvents :: Keyboard -> IO Keyboard
parseEvents keysDown = do
  event <- SDL.pollEvent
  case event of
    Nothing -> return keysDown
    Just (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ k))) ->
      parseEvents $ S.insert k keysDown
    Just (SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ k))) ->
      parseEvents $ S.delete k keysDown
    _ -> parseEvents keysDown

keyDown :: SDL.Keycode -> Keyboard -> Bool
keyDown k = not . null . S.filter ((== k) . SDL.keysymKeycode)
