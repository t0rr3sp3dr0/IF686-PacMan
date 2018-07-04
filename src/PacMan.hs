module PacMan where

    import Control.Concurrent
    import Control.Concurrent.STM
    import Control.Monad
    import Data.Map
    import Data.Maybe
    import Prelude hiding (Left, lookup, Right)

    import qualified SDL
    import qualified SDL.Image
    import qualified SDL.Mixer

    import qualified Base
    import Maze

    type PacManTextureMap = Map PacManTextureKey PacManTexture
    type PacManTextureKey = PacManState
    type PacManTexture = Base.Point

    data PacManState = Right | Left | Up | Down | Dead
      deriving (Bounded, Enum, Eq, Ord, Show)

    type PacManFrame = Int

    data PacMan = PacMan { state :: TVar PacManState, frame :: TVar PacManFrame, point :: TVar Base.Point, textures :: TVar PacManTextureMap, sprites :: SDL.Texture, collisionDetection :: TVar (() -> STM ()), triggerSound :: TVar Bool, coinsEaten :: TVar (Map Base.Point ()), eatCoin :: TVar (() -> STM ()), powerEnabler :: TVar (() -> STM ()) }

    getState :: PacMan -> STM PacManState
    getState pacMan = readTVar (state pacMan)
    setState :: PacMan -> PacManState -> STM ()
    setState pacMan newState = do
        actualState <- getState pacMan
        when (actualState /= Dead) (writeTVar (state pacMan) newState)

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

    getCollisionDetection :: PacMan -> STM (() -> STM ())
    getCollisionDetection pacMan = readTVar (collisionDetection pacMan)
    setCollisionDetection :: PacMan -> (() -> STM ()) -> STM ()
    setCollisionDetection pacMan = writeTVar (collisionDetection pacMan)

    getTriggerSound :: PacMan -> STM Bool
    getTriggerSound pacMan = readTVar (triggerSound pacMan)
    setTriggerSound :: PacMan -> Bool -> STM ()
    setTriggerSound pacMan = writeTVar (triggerSound pacMan)
    
    getCoinsEaten :: PacMan -> STM (Map Base.Point ())
    getCoinsEaten pacMan = readTVar (coinsEaten pacMan)
    setCoinsEaten :: PacMan -> Map Base.Point () -> STM ()
    setCoinsEaten pacMan = writeTVar (coinsEaten pacMan)

    getEatCoin :: PacMan -> STM (() -> STM ())
    getEatCoin pacMan = readTVar (eatCoin pacMan)
    setEatCoin :: PacMan -> (() -> STM ()) -> STM ()
    setEatCoin pacMan = writeTVar (eatCoin pacMan)

    getPowerEnabler :: PacMan -> STM (() -> STM ())
    getPowerEnabler pacMan = readTVar (powerEnabler pacMan)
    setPowerEnabler :: PacMan -> (() -> STM ()) -> STM ()
    setPowerEnabler pacMan = writeTVar (powerEnabler pacMan)

    newPacMan :: SDL.Texture -> STM PacMan
    newPacMan sprites = do
        state <- newTVar Right
        frame <- newTVar 0
        point <- newTVar (Base.Point2D 0 0)
        textures <- newTVar (fromList [])
        collisionDetection <- newTVar return
        triggerSound <- newTVar False
        coinsEaten <- newTVar (fromList [])
        eatCoin <- newTVar return
        powerEnabler <- newTVar return
        return (PacMan state frame point textures sprites collisionDetection triggerSound coinsEaten eatCoin powerEnabler)

    movePacMan :: PacMan -> Base.Direction -> STM ()
    movePacMan pacMan direction = do
        point <- getPoint pacMan
        let (p, s) = case direction of {
            Base.Up    -> (Base.Point2D (Base.x point) (Base.y point - constant), Up);
            Base.Down  -> (Base.Point2D (Base.x point) (Base.y point + constant), Down);
            Base.Left  -> (Base.Point2D (Base.x point - constant) (Base.y point), Left);
            Base.Right -> (Base.Point2D (Base.x point + constant) (Base.y point), Right);
            _          -> (point, Dead) }
        setState pacMan s
        let (i, j) = (transform $ Base.y p, transform $ Base.x p)
        when (isValidPosition i j && mazeMatrix !! i !! j /= Door) (do
            setPoint pacMan p
            when (mazeMatrix !! i !! j == Transport) (setPoint pacMan (Base.Point2D (invert $ transport j) (Base.y p)))
            m <- getCoinsEaten pacMan
            when ((mazeMatrix !! i !! j == Coin || mazeMatrix !! i !! j == Power) && isNothing (lookup p m)) (do
                setCoinsEaten pacMan (insert p () m)
                setTriggerSound pacMan True
                e <- getPowerEnabler pacMan
                when (mazeMatrix !! i !! j == Power) (e ()))
            collisionDetection <- getCollisionDetection pacMan
            collisionDetection ())
        where transform a = div (a - offset) constant
              invert a = a * constant + offset
              offset = 18
              constant = 24

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

    walk :: PacMan -> IO ()
    walk pacMan = do
        atomically (do
            state <- getState pacMan
            movePacMan pacMan (stateToDirection state))
        threadDelay 250000
        walk pacMan
        where
            constant = 24

    onDeath :: PacMan -> IO ()
    onDeath pacMan = do
        state <- atomically (getState pacMan)
        if state /= Dead
            then onDeath pacMan
            else do
                SDL.Mixer.openAudio SDL.Mixer.defaultAudio 256
                SDL.Mixer.load "./resources/audios/death.wav" >>= SDL.Mixer.play

    onEatCoin :: PacMan -> IO ()
    onEatCoin pacMan = do
        b <- atomically (do
            b <- getTriggerSound pacMan
            setTriggerSound pacMan False
            return b)
        when b (do
            SDL.Mixer.openAudio SDL.Mixer.defaultAudio 256
            SDL.Mixer.load "./resources/audios/chomp.wav" >>= SDL.Mixer.play
            threadDelay 875000)
        onEatCoin pacMan

    directionToState :: Base.Direction -> PacManState
    directionToState direction = case direction of
        Base.Up    -> Up
        Base.Down  -> Down
        Base.Left  -> Left
        Base.Right -> Right
        _          -> Dead

    stateToDirection :: PacManState -> Base.Direction
    stateToDirection state = case state of
        Up    -> Base.Up
        Down  -> Base.Down
        Left  -> Base.Left
        Right -> Base.Right
        _     -> Base.None

    instance Base.Entity PacMan where
        initialize pacMan renderer = do
            forkIO $ walk pacMan
            forkIO $ onDeath pacMan
            forkIO $ nextFrame pacMan
            forkIO $ onEatCoin pacMan
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
