module Intent where

import qualified SDL

import Base

data Intent = Move Direction | Idle | Quit

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
    SDL.KeycodeUp     -> Move Base.Up
    SDL.KeycodeDown   -> Move Base.Down
    SDL.KeycodeLeft   -> Move Base.Left
    SDL.KeycodeRight  -> Move Base.Right
    _                 -> Move Base.None

runIntent :: (Monad m) => m () -> (Direction -> m ()) -> Intent -> m Bool
runIntent _ _ Quit = pure False
runIntent r _ Idle = True <$ r
runIntent _ move (Move direction) = True <$ move direction
