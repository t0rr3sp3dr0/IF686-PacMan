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
        let g = head ghosts
        s <- Ghost.getState g
        d <- Ghost.getCanDie g
        when (b && s /= Ghost.Dead) (if s /= Ghost.Mortal && not d
            then PacMan.setState pacMan PacMan.Dead
            else Ghost.setState g Ghost.Dead)
    
    enablePower :: [Ghost.Ghost] -> () -> STM ()
    enablePower [] _ = return ()
    enablePower (g : gs) _ = do
        Ghost.setState g Ghost.Mortal
        enablePower gs ()
