module Lib
where

import Graphics.Gloss
import State (initGameState, BlackjackGame (randomGen))
import ImageLoader
import Rendering
import Handlers
import System.Random


width, height, offset :: Int
width = 750
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
  rndGen <- newStdGen
  play window backgroundColor 30 bjGameState {randomGen = rndGen} drawScreen handleInput updateGame

updateGame :: Float -> BlackjackGame -> BlackjackGame
updateGame timePassed state = state
