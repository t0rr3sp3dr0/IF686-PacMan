module Intent where

import qualified SDL

import Base

data Intent = MovePacMan Direction | MoveGhost Direction | Idle | Quit

newIntent :: Maybe SDL.Event -> Intent
newIntent = maybe Idle (payloadToIntent . extractPayload)

extractPayload :: SDL.Event -> SDL.EventPayload
extractPayload (SDL.Event _t p) = p

payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent         = Quit
payloadToIntent (SDL.KeyboardEvent k) = fromKey k
payloadToIntent _                     = Idle

fromKey :: SDL.KeyboardEventData -> Intent
fromKey (SDL.KeyboardEventData _ SDL.Released _ _) = Idle
fromKey (SDL.KeyboardEventData _ SDL.Pressed _ keysym) = case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit
    SDL.KeycodeW      -> MoveGhost Base.Up
    SDL.KeycodeS      -> MoveGhost Base.Down
    SDL.KeycodeA      -> MoveGhost Base.Left
    SDL.KeycodeD      -> MoveGhost Base.Right
    SDL.KeycodeUp     -> MovePacMan Base.Up
    SDL.KeycodeDown   -> MovePacMan Base.Down
    SDL.KeycodeLeft   -> MovePacMan Base.Left
    SDL.KeycodeRight  -> MovePacMan Base.Right
    _                 -> MovePacMan Base.None

runIntent :: (Monad m) => m () -> (Direction -> m ()) -> (Direction -> m ()) -> Intent -> m Bool
runIntent _ _ _ Quit = pure False
runIntent r _ _ Idle = True <$ r
runIntent _ movePacMan _ (MovePacMan direction) = True <$ movePacMan direction
runIntent _ _ moveGhost (MoveGhost direction) = True <$ moveGhost direction
