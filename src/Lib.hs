module Lib
where

import Graphics.Gloss
import State
import Types
import ImageLoader
import Rendering
import Handlers
import System.Random
import Text.Read
import System.Environment
import MysteriousConstants

configPath :: FilePath
configPath = "config.txt"

argCheck :: [String] -> Maybe Int
argCheck [num] = case readMaybe num of
                                Nothing -> Nothing
                                Just number -> (if number > 0 && number <= 3 then Just number else Nothing)
argCheck _ = Nothing

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
  args <- getArgs
  case argCheck args of
    Nothing -> putStrLn errorMessage
    Just number -> do
      bjGameState <- initGameState number
      images <- loadImages
      rndGen <- newStdGen
      play window backgroundColor 30 bjGameState {randomGen = rndGen} drawScreen handleInput updateGame

updateGame :: Float -> BlackjackGame -> BlackjackGame
updateGame timePassed state = state
