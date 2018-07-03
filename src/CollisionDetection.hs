module CollisionDetection where

    import Control.Concurrent.STM
    import Control.Monad

    import qualified PacMan
    import qualified Ghost

    detectCollision :: PacMan.PacMan -> [Ghost.Ghost] -> () -> STM ()
    detectCollision pacMan ghosts _ = do
        pP <- PacMan.getPoint pacMan
        b <- foldr (\ghost b -> do
            _b <- b
            if _b
                then b
                else do
                    gP <- Ghost.getPoint ghost
                    return (pP == gP)) (return False) ghosts
        when b (PacMan.setState pacMan PacMan.Dead)
