module Base where

    import qualified SDL

    data Point = Point2D { x :: Int, y :: Int }
        deriving (Eq)

    class Entity a where
        initialize :: a -> SDL.Renderer -> IO ()
        render :: a -> SDL.Renderer -> IO ()
        position :: a -> IO Point

    data Direction = Up | Down | Left | Right | None
