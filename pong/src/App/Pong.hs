{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Pong where

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

type Rect = (Pos, App.Pong.Size)

type Rects = [App.Pong.Rect]

type BallPos = Pos

type Velocity = (Double, Double)

data Player
  = Player1
  | Player2

ballInitPos = (400, 200)

ballSize = (8, 8)

ballInitVel = (-0.1, -0.1)

topWall = 0

bottomWall = 600

batSpeed = 1

batSize = (10, 40)

p1StartPos = 200

p2StartPos = 200

gameUpdateHz = 60

targetSecondsPerFrame = 1000 / gameUpdateHz

data BallBounce
  = HBounce
  | VBounce

vecMul :: Num a => a -> (a, a) -> (a, a)
vecMul c (x, y) = (x * c, y * c)

vecAdd :: Num a => (a, a) -> (a, a) -> (a, a)
vecAdd (a, b) (c, d) = (a + c, b + d)

bounce :: Velocity -> BallBounce -> Velocity
bounce (dx, dy) b =
  case b of
    HBounce -> (- dx, dy)
    VBounce -> (dx, - dy)

wallBounce :: Coroutine BallPos (Event BallBounce)
wallBounce =
  proc blPos -> do
    collisionEvt <-
      watch
        (\(_, y) -> y < topWall || y > bottomWall)
        -<
          blPos
    constE VBounce -< collisionEvt

batBounce :: Coroutine (PlayerPos, PlayerPos, BallPos) (Event BallBounce)
batBounce =
  proc (pl1Pos, pl2Pos, blPos) -> do
    collisionEvt <- watch collision -< (pl1Pos, pl2Pos, blPos)
    constE HBounce -< collisionEvt

collision :: (PlayerPos, PlayerPos, BallPos) -> Bool
collision ((p1x, p1y), (p2x, p2y), (bx, by)) =
  (abs (p1x - bx) < w' && abs (p1y - by) < h')
    || (abs (p2x - bx) < w' && abs (p2y - by) < h')
  where
    w' = (bw + pw) / 2
    h' = (bh + ph) / 2
    (bw, bh) = ballSize
    (pw, ph) = batSize

mkRect :: Pos -> App.Pong.Size -> App.Pong.Rect
mkRect (x, y) (w, h) = ((x - w', y - h'), (w, h))
  where
    w' = w / 2
    h' = h / 2

game :: Coroutine Keyboard [App.Pong.Rect]
game =
  proc kb -> do
    pl1Pos <- p1PlayerPos -< kb
    pl2Pos <- p2PlayerPos -< kb
    blPos <- resettingBallPos -< (pl1Pos, pl2Pos)
    returnA
      -<
        [ mkRect pl1Pos batSize,
          mkRect pl2Pos batSize,
          mkRect blPos ballSize
        ]

p1PlayerPos :: Coroutine Keyboard PlayerPos
p1PlayerPos =
  proc kb -> do
    spd <- playerSpeed -< (Player1, kb)
    y <- integrate p1StartPos -< spd
    returnA -< (10, y)

p2PlayerPos :: Coroutine Keyboard PlayerPos
p2PlayerPos =
  proc kb -> do
    spd <- playerSpeed -< (Player2, kb)
    y <- integrate p2StartPos -< spd
    returnA -< (790, y)

playerSpeed :: Coroutine (Player, Keyboard) Double
playerSpeed = arr keyboardDir
  where
    keyboardDir (Player1, kb)
      | keyDown GLFW.Key'W kb = - batSpeed
      | keyDown GLFW.Key'S kb = batSpeed
      | otherwise = 0
    keyboardDir (Player2, kb)
      | keyDown GLFW.Key'Up kb = - batSpeed
      | keyDown GLFW.Key'Down kb = batSpeed
      | otherwise = 0

ballPos :: Coroutine (PlayerPos, PlayerPos) BallPos
ballPos =
  proc (pl1Pos, pl2Pos) -> do
    rec bounces <-
          (batBounce -< (pl1Pos, pl2Pos, pos)) <++>
            (wallBounce -< pos)
        vel <- scanE bounce ballInitVel -< bounces
        pos <- delay ballInitPos <<< scan vecAdd ballInitPos -< vel
    returnA -< pos

resettingBallPos :: Coroutine (PlayerPos, PlayerPos) BallPos
resettingBallPos =
  proc (pl1Pos, pl2Pos) -> do
    rec pos <- restartWhen ballPos -< ((pl1Pos, pl2Pos), reset)
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
  Window ->
  IORef Keyboard ->
  Counter ->
  Rects ->
  Coroutine Keyboard Rects ->
  IO ()
appLoop window kb prevCounter prevRects gameLogic = do
  kb' <- readIORef kb
  let (newRects, gameLogic') = step gameLogic kb'
  GLFW.pollEvents
  draw window newRects
  curCounter <- GLFW.getTimerValue
  elapsedTime <- getSecondsElapsed prevCounter curCounter
  when (elapsedTime < targetSecondsPerFrame) $
    threadDelay $ floor $ targetSecondsPerFrame - elapsedTime
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

drawRect :: Window -> App.Pong.Rect -> IO ()
drawRect window (rPos, rSize) = do
  let (x, y) = rPos
  let (w, h) = rSize
  color3f 1 1 1
  GL.rect (GL.Vertex2 x y) (GL.Vertex2 (x + w) (y + h))

keyPressed :: IORef Keyboard -> GLFW.KeyCallback
keyPressed kb win k _ GLFW.KeyState'Pressed _ = modifyIORef kb $ S.insert k
keyPressed kb win k _ GLFW.KeyState'Released _ = modifyIORef kb $ S.delete k
keyPressed _ _ _ _ _ _ = return ()

keyDown :: Key -> Keyboard -> Bool
keyDown k = not . null . S.filter (== k)
