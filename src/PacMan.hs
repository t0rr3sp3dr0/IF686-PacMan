module PacMan where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Map
import Prelude hiding (Left, lookup, Right)

import qualified SDL
import qualified SDL.Image
import qualified SDL.Mixer

import qualified Base

type PacManTextureMap = Map PacManTextureKey PacManTexture
type PacManTextureKey = PacManState
type PacManTexture = Base.Point

data PacManState = Right | Left | Up | Down | Dead
  deriving (Bounded, Enum, Eq, Ord, Show)

type PacManFrame = Int

data PacMan = PacMan { state :: TVar PacManState, frame :: TVar PacManFrame, point :: TVar Base.Point, textures :: TVar PacManTextureMap, sprites :: SDL.Texture }

getState :: PacMan -> STM PacManState
getState pacMan = readTVar (state pacMan)
setState :: PacMan -> PacManState -> STM ()
setState pacMan = writeTVar (state pacMan)

getFrame :: PacMan -> STM PacManFrame
getFrame pacMan = readTVar (frame pacMan)
setFrame :: PacMan -> PacManFrame -> STM ()
setFrame pacMan = writeTVar (frame pacMan)

getPoint :: PacMan -> STM Base.Point
getPoint pacMan = readTVar (point pacMan)
setPoint :: PacMan -> Base.Point -> STM ()
setPoint pacMan = writeTVar (point pacMan)

getTextures :: PacMan -> STM PacManTextureMap
getTextures pacMan = readTVar (textures pacMan)
setTextures :: PacMan -> PacManTextureMap -> STM ()
setTextures pacMan = writeTVar (textures pacMan)

newPacMan :: SDL.Texture -> STM PacMan
newPacMan sprites = do
    state <- newTVar Right
    frame <- newTVar 0
    point <- newTVar (Base.Point2D 0 0)
    textures <- newTVar (fromList [])
    return (PacMan state frame point textures sprites)

movePacMan :: PacMan -> Base.Direction -> STM ()
movePacMan pacMan direction = do
    let constant = 24
    point <- getPoint pacMan
    let (p, s) = case direction of {
        Base.Up    -> (Base.Point2D (Base.x point) (Base.y point - constant), Up);
        Base.Down  -> (Base.Point2D (Base.x point) (Base.y point + constant), Down);
        Base.Left  -> (Base.Point2D (Base.x point - constant) (Base.y point), Left);
        Base.Right -> (Base.Point2D (Base.x point + constant) (Base.y point), Right);
        _          -> (point, Dead) }
    setPoint pacMan p
    setState pacMan s

nextFrame :: PacMan -> IO ()
nextFrame pacMan = do
    (s, f) <- atomically (do
        state <- getState pacMan
        frame <- getFrame pacMan
        let newFrame = mod (frame + 1) $ if state == Dead
            then 12
            else 2
        setFrame pacMan newFrame
        return (state, newFrame))
    threadDelay 125000
    when (s /= Dead || f /= 11) $ nextFrame pacMan

onDeath :: PacMan -> IO ()
onDeath pacMan = do
    state <- atomically (getState pacMan)
    if state /= Dead
        then onDeath pacMan
        else do
            SDL.Mixer.openAudio SDL.Mixer.defaultAudio 256
            SDL.Mixer.load "./resources/audios/death.wav" >>= SDL.Mixer.play

instance Base.Entity PacMan where
    initialize pacMan renderer = do
        forkIO $ onDeath pacMan
        forkIO $ nextFrame pacMan
        atomically (do
            setState pacMan Right
            setFrame pacMan 0
            setPoint pacMan (Base.Point2D 18 18)
            setTextures pacMan (fromList [(Right, Base.Point2D 456 0), (Left, Base.Point2D 456 16), (Up, Base.Point2D 456 32), (Down, Base.Point2D 456 48), (Dead, Base.Point2D 488 0)]))
            
    render pacMan renderer = do
        (state, frame, point, textures) <- atomically (do
            state <- getState pacMan
            frame <- getFrame pacMan
            point <- getPoint pacMan
            textures <- getTextures pacMan
            return (state, frame, point, textures))
        let Just texture = lookup state textures
        let (i, j) = (fromIntegral (Base.x texture + 16 * frame), fromIntegral $ Base.y texture)
        let (x, y) = (fromIntegral $ Base.x point, fromIntegral $ Base.y point)
        SDL.copy renderer (sprites pacMan) (Just $ SDL.Rectangle (SDL.P (SDL.V2 i j)) (SDL.V2 16 16)) (Just $ SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 36 36))

    position pacMan = atomically (getPoint pacMan)