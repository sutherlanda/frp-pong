{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Pong where

import Control.Arrow
import Control.Concurrent
import Control.Monad (when)
import Data.Bifunctor (bimap)
import Data.IORef
import qualified Data.Set as S
import Data.Time.Clock.POSIX
import Data.Word
import GHC.Float
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Lib.Coroutine
import System.Exit (exitSuccess)

type Counter = Word64

type Keyboard = S.Set Key

type Pos = (Double, Double)

type PlayerPos = Pos

type Size = (Double, Double)

type Rect = (Pos, Pong.Size)

type Rects = [Pong.Rect]

type BallPos = Pos

type Velocity = (Double, Double)

ballInitPos = (400, 200)

ballSize = (8, 8)

ballInitVel = (-1, -1)

topWall = 10

bottomWall = 590

batSpeed = 1

batSize = (10, 40)

startPos = 200

gameUpdateHz = 60.0

targetSecondsPerFrame = 1000 / gameUpdateHz

data BallBounce
 = HBounce
 | VBounce

vecMul :: Int -> (Int, Int) -> (Int, Int)
vecMul c (x, y) = (x * c, y * c)

vecAdd :: (Double, Double) -> (Double, Double) -> (Double, Double)
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
    w' = (bw + pw) / 2
    h' = (bh + ph) / 2
    (bw, bh) = ballSize
    (pw, ph) = batSize

mkRect :: Pos -> Pong.Size -> Pong.Rect
mkRect (x, y) (w, h) = ((x - w', y - h'), (w, h))
  where
    w' = w / 2
    h' = h / 2

game :: Coroutine Keyboard [Pong.Rect]
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

playerSpeed :: Coroutine Keyboard Double
playerSpeed = arr keyboardDir
  where
    keyboardDir kb
     | keyDown GLFW.Key'Up kb = -batSpeed
     | keyDown GLFW.Key'Down kb = batSpeed
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
  GLFW.init
  GLFW.defaultWindowHints
  Just win <- GLFW.createWindow 800 600 "FRP Pong" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.swapInterval 0 -- Don't vsync, so we'll have to set up a framerate limit manually
  GLFW.setWindowSizeCallback win (Just resizeWindow)
  kb <- newIORef S.empty
  GLFW.setKeyCallback win (Just $ keyPressed kb)
  GLFW.setWindowCloseCallback win (Just shutdown)
  resizeWindow win 800 600
  counter <- GLFW.getTimerValue
  appLoop win kb counter [] game
  GLFW.destroyWindow win
  GLFW.terminate

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

shutdown :: GLFW.WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess

getSecondsElapsed :: Counter -> Counter -> IO Double
getSecondsElapsed prevCounter curCounter = do
  freq <- GLFW.getTimerFrequency
  let cur = fromIntegral curCounter
  let prev = fromIntegral prevCounter
  return $ (cur - prev) / fromIntegral freq

appLoop ::
    Window
 -> IORef Keyboard
 -> Counter
 -> Rects
 -> Coroutine Keyboard Rects
 -> IO ()
appLoop window kb prevCounter prevRects gameLogic = do
  kb' <- readIORef kb
  let (newRects, gameLogic') = step gameLogic kb'
  GLFW.pollEvents
  draw window newRects
  curCounter <- GLFW.getTimerValue
  elapsedTime <- getSecondsElapsed prevCounter curCounter
  when (elapsedTime < targetSecondsPerFrame) $ do
   print $ floor $ (targetSecondsPerFrame - elapsedTime) * 1000
   threadDelay $ floor $ (targetSecondsPerFrame - elapsedTime) * 1000
  GLFW.swapBuffers window
  appLoop window kb curCounter newRects gameLogic'

draw :: Window -> Rects -> IO ()
draw window rects = do
  GL.clearColor $= GL.Color4 0 0 0 255
  GL.clear [GL.ColorBuffer]
  mapM_ (drawRect window) rects

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z

drawRect :: Window -> Pong.Rect -> IO ()
drawRect window (rPos, rSize) = do
  let (x, y) = rPos
  let (w, h) = rSize
  color3f 1 1 1
  GL.rect
   (GL.Vertex2 (x - w / 2) (y + h / 2))
   (GL.Vertex2 (x + w / 2) (y - h / 2))

keyPressed :: IORef Keyboard -> GLFW.KeyCallback
keyPressed kb win k _ GLFW.KeyState'Pressed _ = modifyIORef kb $ S.insert k
keyPressed kb win k _ GLFW.KeyState'Released _ = modifyIORef kb $ S.delete k
keyPressed _ _ _ _ _ _ = return ()

keyDown :: Key -> Keyboard -> Bool
keyDown k = not . null . S.filter (== k)
