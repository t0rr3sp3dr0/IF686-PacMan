module Maze where

import qualified SDL

renderMaze :: SDL.Renderer -> SDL.Texture -> IO ()
renderMaze renderer texture = SDL.copy renderer texture mask placement
    where mask = Just $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 width height)
          placement = Just $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 (width * constant) (height * constant))
          constant = 3
          width = 224
          height = 248
