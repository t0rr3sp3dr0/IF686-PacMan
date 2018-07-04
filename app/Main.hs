{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Extra
import Data.Map

import SDL (($=))
import qualified SDL.Image
import qualified SDL.Mixer

import Base
import Intent
import GUI
import Maze
import qualified PacMan
import qualified Ghost
import CollisionDetection

main :: IO ()
main = withSDL $ withSDLImage $ do
  setHintQuality
  withWindow "PacMan" (672, 744) $ \w ->
    withRenderer w $ \r -> do
      SDL.Mixer.openAudio SDL.Mixer.defaultAudio 256
      SDL.Mixer.load "./resources/audios/beginning.wav" >>= SDL.Mixer.play
      
      texture <- SDL.Image.loadTexture r "./resources/images/sprites.png"

      ghost <- atomically (Ghost.newGhost texture)
      initialize ghost r

      pacMan <- atomically (PacMan.newPacMan texture)
      initialize pacMan r

      atomically (do
        let collisionDetection = detectCollision pacMan [ghost]
        PacMan.setCollisionDetection pacMan collisionDetection
        Ghost.setCollisionDetection ghost collisionDetection
        let powerEnabler = enablePower [ghost]
        PacMan.setPowerEnabler pacMan powerEnabler)

      let render = draw r pacMan ghost texture
      let movePacMan direction = atomically (PacMan.setState pacMan (PacMan.directionToState direction))
      let moveGhost direction = atomically (Ghost.setState ghost (Ghost.directionToState direction))
      whileM $ newIntent <$> SDL.pollEvent >>= runIntent render movePacMan moveGhost

      SDL.destroyTexture texture

draw :: SDL.Renderer -> PacMan.PacMan -> Ghost.Ghost -> SDL.Texture -> IO ()
draw r pacMan ghost texture = do
  SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
  SDL.clear r

  renderMaze r texture
  hideCoins r pacMan
  render ghost r
  render pacMan r

  SDL.present r

hideCoins :: SDL.Renderer -> PacMan.PacMan -> IO ()
hideCoins r pacMan = do
  coinsEaten <- atomically (PacMan.getCoinsEaten pacMan)
  drawBlack r (PacMan.sprites pacMan) (toList coinsEaten)

drawBlack :: SDL.Renderer -> SDL.Texture -> [(Point, ())] -> IO ()
drawBlack r sprites [] = return ()
drawBlack r sprites ((point, _) : xs) = do
  let texture = Base.Point2D 664 232
  let (i, j) = (fromIntegral $ Base.x texture, fromIntegral $ Base.y texture)
  let (x, y) = (fromIntegral $ Base.x point, fromIntegral $ Base.y point)
  SDL.copy r sprites (Just $ SDL.Rectangle (SDL.P (SDL.V2 i j)) (SDL.V2 16 16)) (Just $ SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 36 36))
  drawBlack r sprites xs
