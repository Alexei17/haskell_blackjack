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
initInitialPlayer = Player { hand = emptyHand, balance = 0, currentBet = 0, playerPos = 0 }

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
    currentBet :: Int,
    playerPos :: Int
} deriving (Show)

data Hand = Hand {
    size :: Int,
    cards :: [Card]
} deriving (Show)

data Card = Card {
              cardRank :: CardRank,
              cardSuit :: CardSuit
} deriving (Eq, Show, Ord)

instance Enum Card where
  toEnum index = Card
    { cardRank = toEnum $ index `div` 4
    , cardSuit     = toEnum $ index `mod` 4
    }
  fromEnum card = fromEnum (cardRank card) * 4 + fromEnum (cardSuit card)


data CardRank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ace
    | Jack
    | King
    | Queen 
    | Ten
    deriving (Eq, Ord, Bounded, Enum, Show)

data CardSuit
    = Clubs
    | Diamonds
    | Hearts
    | Spades
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
