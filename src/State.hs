{-# OPTIONS_GHC -Wno-missing-fields #-}
module State where
import ImageLoader
import Data.List
import System.Random.Shuffle (shuffle')
import System.Random
import Control.Monad
import Types
import MysteriousConstants

initGameState :: Int -> IO BlackjackGame
initGameState n = createGameStateWith n <$> loadImages

createGameStateWith :: Int -> Images -> BlackjackGame
createGameStateWith n imgs = Game {
    players = map initInitialPlayer [0..n-1],
    dealer = initInitialPlayer 0,
    gameState = GameOver,
    images = imgs,
    deck = Hand { size = 0, cards = [] },
    selectedPlayer = 0,
    slider = initSlider
    }

initInitialPlayer :: Int -> Player
initInitialPlayer n = Player { hand = emptyHand, balance = 1000, currentBet = 0, playerPos = n, finished = False, bust = False, hasWon = NotDetermined }

emptyHand :: Hand
emptyHand = Hand { cards = [] , size = 0 }

initSlider :: Slider
initSlider = Slider {
    ballPos = -150,
    isSelected = False,
    val = 0,
    minVal = 0,
    maxVal = 1000,
    step = 2
}

shuffle gen [] = []
shuffle gen list = randomElem : shuffle newGen newList
  where
   randomTuple = randomR (0,length list - 1) gen
   randomIndex = fst randomTuple
   newGen      = snd randomTuple
   randomElem  = list !! randomIndex
   newList     = take randomIndex list ++ drop (randomIndex+1) list


createShuffledDeck :: BlackjackGame -> Hand
createShuffledDeck state = Hand {
    size = 52,
    cards = shuffle (randomGen state) (Card <$> cardRanks <*> cardSuits)
}

getNumberByHand :: Hand -> Int
getNumberByHand hand
  | acesCount < 2 =
    if countMaxAce > 21 then countMinAce else countMaxAce
  | acesCount >= 2 =
      if countAcesAs cards_ (11 + acesCount - 1) > 21 -- AA = 12 | 2, AAA = 13 | 3 etc.
        then countAcesAs cards_ acesCount
        else countAcesAs cards_ (11 + acesCount - 1)
  | otherwise = 100
  where
      cards_ = cards hand
      acesCount = length (filter (\ card -> cardRank card == Ace) (cards hand))
      countMaxAce = sum (map (`getNumberByCard` True) (cards hand))
      countMinAce = sum (map (`getNumberByCard` False) (cards hand))
      countTwoAces_as2
        = 2 + sum (map (`getNumberByCard` False) (cards hand))


countAcesAs :: [Card] -> Int -> Int
countAcesAs cards n = n + sum (map (`getNumberByCard` False) handWithoutAces)
    where
    handWithoutAces = filter (\card -> cardRank card /=  Ace) cards

getNumberByCard :: Card -> Bool -> Int
getNumberByCard card maxAce = case cardRank card of
  Two -> 2
  Three -> 3
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ace -> if maxAce then 11 else 1
  Jack -> 10
  King -> 10
  Queen -> 10
  Ten -> 10
