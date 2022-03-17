module Handlers where
import State
import Graphics.Gloss.Interface.IO.Game

handleInput :: Event -> BlackjackGame -> BlackjackGame
handleInput event state = state
