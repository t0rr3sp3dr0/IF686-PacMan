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
            mask <- SDL.Image.loadTexture r "./resources/images/mask.png"
            end <- SDL.Image.loadTexture r "./resources/images/end.png"

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

            m <- newEmptyMVar
            t <- atomically (newTVar 0)
            forkIO $ levels m t pacMan ghost

            let render = draw r pacMan ghost texture mask end m t
            let movePacMan direction = atomically (do {
                level <- readTVar t;
                PacMan.setState pacMan (PacMan.directionToState (transformDirection direction level)); })
            let moveGhost direction = atomically (Ghost.setState ghost (Ghost.directionToState direction))
            whileM $ newIntent <$> SDL.pollEvent >>= runIntent render movePacMan moveGhost

            SDL.destroyTexture end
            SDL.destroyTexture mask
            SDL.destroyTexture texture

draw :: SDL.Renderer -> PacMan.PacMan -> Ghost.Ghost -> SDL.Texture -> SDL.Texture -> SDL.Texture -> MVar () -> TVar Int -> IO ()
draw r pacMan ghost texture mask end m t = do
    SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
    SDL.clear r

    level <- readTVarIO t

    if level > 2
        then SDL.copy r end (Just $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 1344 1488)) (Just $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 672 744))
        else do
            renderMaze r texture
            hideCoins r pacMan m
            when (level == 1) (renderMask r mask pacMan ghost)
    render ghost r
    render pacMan r

    SDL.present r

hideCoins :: SDL.Renderer -> PacMan.PacMan -> MVar () -> IO ()
hideCoins r pacMan m = do
    coinsEaten <- atomically (PacMan.getCoinsEaten pacMan)
    let coins = toList coinsEaten
    drawBlack r (PacMan.sprites pacMan) coins
    when (length coins == 244) (putMVar m ())

drawBlack :: SDL.Renderer -> SDL.Texture -> [(Point, ())] -> IO ()
drawBlack r sprites [] = return ()
drawBlack r sprites ((point, _) : xs) = do
    let texture = Base.Point2D 664 232
    let (i, j) = (fromIntegral $ Base.x texture, fromIntegral $ Base.y texture)
    let (x, y) = (fromIntegral $ Base.x point, fromIntegral $ Base.y point)
    SDL.copy r sprites (Just $ SDL.Rectangle (SDL.P (SDL.V2 i j)) (SDL.V2 16 16)) (Just $ SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 36 36))
    drawBlack r sprites xs

levels :: MVar () -> TVar Int -> PacMan.PacMan -> Ghost.Ghost -> IO ()
levels m t pacMan ghost = do
    takeMVar m
    i <- atomically (do
        PacMan.setCoinsEaten pacMan (fromList [])
        Ghost.setCanDie ghost False
        writeTVar (Ghost.state ghost) Ghost.Up
        i <- readTVar t
        writeTVar t (i + 1)
        return i)
    when (i /= 2) (levels m t pacMan ghost)

renderMask :: SDL.Renderer -> SDL.Texture -> PacMan.PacMan -> Ghost.Ghost -> IO ()
renderMask r mask pacMan ghost = do
    point <- atomically (do
        state <- Ghost.getState ghost
        if state /= Ghost.Dead
            then Ghost.getPoint ghost
            else PacMan.getPoint pacMan)
    let (x, y) = (fromIntegral (Base.x point) - fromIntegral (div w 2), fromIntegral (Base.y point) - fromIntegral (div h 2))
    SDL.copy r mask (Just $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 w h)) (Just $ SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h))
    where w = 1344
          h = 1488

transformDirection :: Base.Direction -> Int -> Base.Direction
transformDirection direction level = if level /= 2
    then direction
    else case direction of
        Base.Up    -> Base.Right
        Base.Down  -> Base.Left
        Base.Left  -> Base.Down
        Base.Right -> Base.Up
        _          -> direction
