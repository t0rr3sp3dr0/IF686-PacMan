{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL

import Control.Concurrent.STM
import Control.Monad.Extra

import SDL (($=))
import qualified SDL.Image
import qualified SDL.Mixer

import Base
import Intent
import GUI
import Maze
import PacMan

main :: IO ()
main = withSDL $ withSDLImage $ do
  setHintQuality
  withWindow "PacMan" (672, 744) $ \w ->
    withRenderer w $ \r -> do
      SDL.Mixer.openAudio SDL.Mixer.defaultAudio 256
      SDL.Mixer.load "./resources/audios/beginning.wav" >>= SDL.Mixer.play
      
      texture <- SDL.Image.loadTexture r "./resources/images/sprites.png"

      pacMan <- atomically (newPacMan texture)
      initialize pacMan r

      let render = draw r pacMan texture
      let move direction = atomically (setState pacMan (directionToState direction))
      whileM $ newIntent <$> SDL.pollEvent >>= runIntent render move

      SDL.destroyTexture texture

draw :: SDL.Renderer -> PacMan -> SDL.Texture -> IO ()
draw r pacMan texture = do
  SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
  SDL.clear r

  renderMaze r texture
  render pacMan r

  SDL.present r
