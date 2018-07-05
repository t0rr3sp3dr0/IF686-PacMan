module Ghost where

    import Control.Concurrent
    import Control.Concurrent.STM
    import Control.Monad
    import Data.Map
    import Prelude hiding (Left, lookup, Right)

    import qualified SDL
    import qualified SDL.Image
    import qualified SDL.Mixer

    import qualified Base
    import Maze

    type GhostTextureMap = Map GhostTextureKey GhostTexture
    type GhostTextureKey = GhostState
    type GhostTexture = Base.Point

    data GhostState = Right | Left | Up | Down | Mortal | Dead
      deriving (Bounded, Enum, Eq, Ord, Show)

    type GhostFrame = Int

    data Ghost = Ghost { state :: TVar GhostState, frame :: TVar GhostFrame, point :: TVar Base.Point, textures :: TVar GhostTextureMap, sprites :: SDL.Texture, collisionDetection :: TVar (() -> STM ()), canDie :: TVar Bool }

    getState :: Ghost -> STM GhostState
    getState ghost = readTVar (state ghost)
    setState :: Ghost -> GhostState -> STM ()
    setState ghost newState = do
        actualState <- getState ghost
        when (actualState /= Dead) (if newState == Mortal
            then setCanDie ghost True
            else do
                when (newState == Dead) (setCanDie ghost False)
                writeTVar (state ghost) newState)

    getFrame :: Ghost -> STM GhostFrame
    getFrame ghost = readTVar (frame ghost)
    setFrame :: Ghost -> GhostFrame -> STM ()
    setFrame ghost = writeTVar (frame ghost)

    getPoint :: Ghost -> STM Base.Point
    getPoint ghost = readTVar (point ghost)
    setPoint :: Ghost -> Base.Point -> STM ()
    setPoint ghost = writeTVar (point ghost)

    getTextures :: Ghost -> STM GhostTextureMap
    getTextures ghost = readTVar (textures ghost)
    setTextures :: Ghost -> GhostTextureMap -> STM ()
    setTextures ghost = writeTVar (textures ghost)

    getCollisionDetection :: Ghost -> STM (() -> STM ())
    getCollisionDetection ghost = readTVar (collisionDetection ghost)
    setCollisionDetection :: Ghost -> (() -> STM ()) -> STM ()
    setCollisionDetection ghost = writeTVar (collisionDetection ghost)

    getCanDie :: Ghost -> STM Bool
    getCanDie ghost = readTVar (canDie ghost)
    setCanDie :: Ghost -> Bool -> STM ()
    setCanDie ghost = writeTVar (canDie ghost)

    newGhost :: SDL.Texture -> STM Ghost
    newGhost sprites = do
        state <- newTVar Up
        frame <- newTVar 0
        point <- newTVar (Base.Point2D 12 12)
        textures <- newTVar (fromList [])
        collisionDetection <- newTVar return
        canDie <- newTVar False
        return (Ghost state frame point textures sprites collisionDetection canDie)

    moveGhost :: Ghost -> Base.Direction -> STM ()
    moveGhost ghost direction = do
        point <- getPoint ghost
        state <- getState ghost
        let (p, s) = case direction of {
            Base.Up    -> (Base.Point2D (Base.x point) (Base.y point - constant), Up);
            Base.Down  -> (Base.Point2D (Base.x point) (Base.y point + constant), Down);
            Base.Left  -> (Base.Point2D (Base.x point - constant) (Base.y point), Left);
            Base.Right -> (Base.Point2D (Base.x point + constant) (Base.y point), Right);
            _          -> (point, state) }
        setState ghost s
        let (i, j) = (transform $ Base.y p, transform $ Base.x p)
        when (isValidPosition i j) (do
            setPoint ghost p
            collisionDetection <- getCollisionDetection ghost
            collisionDetection ())
        where transform a = div (a - offset) constant
              invert a = a * constant + offset
              offset = 18
              constant = 24

    nextFrame :: Ghost -> IO ()
    nextFrame ghost = do
        (s, f) <- atomically (do
            state <- getState ghost
            frame <- getFrame ghost
            let newFrame = mod (frame + 1) 2
            setFrame ghost newFrame
            return (state, newFrame))
        threadDelay 125000
        when (s /= Dead || f /= 1) $ nextFrame ghost

    walk :: Ghost -> IO ()
    walk ghost = do
        atomically (do
            state <- getState ghost
            moveGhost ghost (stateToDirection state))
        threadDelay 250000
        walk ghost
        where
            constant = 24

    onDeath :: Ghost -> IO ()
    onDeath ghost = do
        state <- atomically (getState ghost)
        when (state == Dead) (do
            SDL.Mixer.openAudio SDL.Mixer.defaultAudio 256
            SDL.Mixer.load "./resources/audios/eatghost.wav" >>= SDL.Mixer.play)
        onDeath ghost
    
    onPower :: Ghost -> IO ()
    onPower ghost = do
        atomically (do
            canDie <- getCanDie ghost
            unless canDie retry)
        threadDelay 8000000
        atomically (setCanDie ghost False)
        onPower ghost

    directionToState :: Base.Direction -> GhostState
    directionToState direction = case direction of
        Base.Up    -> Up
        Base.Down  -> Down
        Base.Left  -> Left
        Base.Right -> Right
        _          -> Dead

    stateToDirection :: GhostState -> Base.Direction
    stateToDirection state = case state of
        Up    -> Base.Up
        Down  -> Base.Down
        Left  -> Base.Left
        Right -> Base.Right
        _     -> Base.None

    instance Base.Entity Ghost where
        initialize ghost renderer = do
            forkIO $ walk ghost
            forkIO $ onDeath ghost
            forkIO $ onPower ghost
            forkIO $ nextFrame ghost
            atomically (do
                setState ghost Up
                setFrame ghost 0
                setPoint ghost (Base.Point2D 306 306)
                setTextures ghost (fromList [(Right, Base.Point2D 456 64), (Left, Base.Point2D 488 64), (Up, Base.Point2D 520 64), (Down, Base.Point2D 552 64), (Mortal, Base.Point2D 584 64), (Dead, Base.Point2D 616 64)]))

        render ghost renderer = do
            (state, frame, point, textures) <- atomically (do
                canDie <- getCanDie ghost
                state <- if canDie
                    then return Mortal
                    else getState ghost
                frame <- getFrame ghost
                point <- getPoint ghost
                textures <- getTextures ghost
                return (state, frame, point, textures))
            let Just texture = lookup state textures
            let (i, j) = (fromIntegral (Base.x texture + 16 * frame), fromIntegral $ Base.y texture)
            let (x, y) = (fromIntegral $ Base.x point, fromIntegral $ Base.y point)
            SDL.copy renderer (sprites ghost) (Just $ SDL.Rectangle (SDL.P (SDL.V2 i j)) (SDL.V2 16 16)) (Just $ SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 36 36))

        position ghost = atomically (getPoint ghost)
