module Lib
where

import Graphics.Gloss
import State (initGameState, BlackjackGame)
import ImageLoader
import Rendering
import Handlers


width, height, offset :: Int
width = 640
height = 1000
offset = 100

window = InWindow "Blackjack" (width, height) (offset, offset)

backgroundColor = makeColorI 4 135 82 255

initialGame = undefined

drawing :: Picture
drawing = circle 80

transformGame _ game = game

runGame :: IO ()
runGame = do
  bjGameState <- initGameState
  images <- loadImages
  play window backgroundColor 30 bjGameState drawScreen handleInput updateGame

updateGame :: Float -> BlackjackGame -> BlackjackGame
updateGame timePassed state = state
