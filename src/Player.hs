module Player where

    import Control.Concurrent.STM
    import Data.Map
    import Prelude hiding (Left, lookup, Right)

    import qualified SDL
    import qualified SDL.Image

    import Base

    type PlayerTextureMap = Map PlayerTextureKey PlayerTexture
    type PlayerTextureKey = (PlayerMode, PlayerState)
    type PlayerTexture = IO (SDL.Texture, SDL.TextureInfo)

    data PlayerMode = Normal -- | Super | Fiery | Invincible
        deriving (Bounded, Enum, Eq, Ord, Show)

    data PlayerState = Standing | Jumping | Running | Crouching | Skidding
        deriving (Bounded, Enum, Eq, Ord, Show)

    getPath :: String -> (PlayerMode, PlayerState) -> FilePath
    getPath name (mode, state) = "./assets/" ++ name ++ "/" ++ show mode ++ show state ++ ".png"

    data Player = Player { name :: String, mode :: TVar PlayerMode, state :: TVar PlayerState, point :: TVar Point, textures :: TVar PlayerTextureMap }

    getMode :: Player -> STM PlayerMode
    getMode player = readTVar (mode player)
    setMode :: Player -> PlayerMode -> STM ()
    setMode player = writeTVar (mode player)

    getState :: Player -> STM PlayerState
    getState player = readTVar (state player)
    setState :: Player -> PlayerState -> STM ()
    setState player = writeTVar (state player)

    getPoint :: Player -> STM Point
    getPoint player = readTVar (point player)
    setPoint :: Player -> Point -> STM ()
    setPoint player = writeTVar (point player)

    getTextures :: Player -> STM PlayerTextureMap
    getTextures player = readTVar (textures player)
    setTextures :: Player -> PlayerTextureMap -> STM ()
    setTextures player = writeTVar (textures player)

    movePlayer :: Player -> Direction -> STM ()
    movePlayer player direction = do
        let constant = 10
        point <- getPoint player
        setPoint player $ case direction of
            Up    -> Point2D (x point) (y point - constant)
            Down  -> Point2D (x point) (y point + constant)
            Left  -> Point2D (x point - constant) (y point)
            Right -> Point2D (x point + constant) (y point)

    newPlayer :: String -> STM Player
    newPlayer name = do
        mode <- newTVar Normal
        state <- newTVar Standing
        point <- newTVar (Point2D 0 0)
        textures <- newTVar (fromList [])
        return (Player name mode state point textures)

    instance Entity Player where
        initialize player renderer = atomically (do
            setMode player Normal
            setState player Standing
            setPoint player (Point2D 0 0)
            setTextures player (fromList [zip player renderer (mode, state) | mode <- [Normal ..], state <- [Standing ..]]))
            where zip player renderer (mode, state) = ((mode, state), do { texture <- SDL.Image.loadTexture renderer (getPath (name player) (mode, state));
                                                                        textureInfo <- SDL.queryTexture texture;
                                                                        return (texture, textureInfo) })

        render player renderer = do
            (mode, state, point, textures) <- atomically (do
                mode <- getMode player
                state <- getState player
                point <- getPoint player
                textures <- getTextures player
                return (mode, state, point, textures))
            let Just playerTexture = lookup (mode, state) textures
            (texture, textureInfo) <- playerTexture
            let (i, j, w, h) = (fromIntegral $ x point, fromIntegral $ y point, SDL.textureWidth textureInfo, SDL.textureHeight textureInfo)
            SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 i j)) (SDL.V2 w h))

        position player = atomically (getPoint player)
