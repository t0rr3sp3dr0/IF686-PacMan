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
import qualified PacMan
import qualified Ghost

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
      atomically (PacMan.addGhost pacMan ghost)

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
  render ghost r
  render pacMan r

  SDL.present r
