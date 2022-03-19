module State where
import ImageLoader
import Data.List
import System.Random.Shuffle (shuffle')
import System.Random
import Control.Monad

initGameState :: IO BlackjackGame
initGameState = createGameStateWith <$> loadImages

createGameStateWith :: Images -> BlackjackGame
createGameStateWith imgs = Game { 
    players = [initInitialPlayer], 
    dealer = initInitialPlayer, 
    gameState = BetPhase,
    images = imgs,
    deck = Hand { size = 0, cards = [] }
    }

initInitialPlayer :: Player
initInitialPlayer = Player { hand = emptyHand, balance = 0, currentBet = 0 }

emptyHand :: Hand
emptyHand = Hand { cards = [] , size = 0 }

data BlackjackGame = Game {
    players :: [Player],
    dealer :: Player,
    gameState :: State,
    images :: Images,
    deck :: Hand,
    randomGen :: StdGen,
    selectedPlayer :: Int
}

data State = BetPhase | Running | GameOver | TakeActionPhase deriving (Eq, Show)
data Player = Player {
    hand :: Hand,
    balance :: Int,
    currentBet :: Int
} deriving (Show)

data Hand = Hand {
    size :: Int,
    cards :: [Card]
} deriving (Show)

data Card = Card {
              cardRank :: CardRank,
              cardSuit :: CardSuit
} deriving (Eq, Show, Ord)

data CardRank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King 
    deriving (Eq, Ord, Bounded, Enum, Show)

data CardSuit
    = Clubs
    | Spades
    | Diamonds
    | Hearts
    deriving (Eq, Ord, Bounded, Enum, Show)

cardRanks :: [CardRank]
cardRanks = [minBound..maxBound]

cardSuits :: [CardSuit]
cardSuits = [minBound..maxBound]


shuffle gen [] = [] 
shuffle gen list = randomElem : shuffle newGen newList
  where 
   randomTuple = randomR (0,(length list) - 1) gen
   randomIndex = fst randomTuple
   newGen      = snd randomTuple
   randomElem  = list !! randomIndex
   newList     = take randomIndex list ++ drop (randomIndex+1) list


createShuffledDeck :: BlackjackGame -> Hand
createShuffledDeck state = Hand {
    size = 52,
    cards = shuffle (randomGen state) (Card <$> cardRanks <*> cardSuits)
}
