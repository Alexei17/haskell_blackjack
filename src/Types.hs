module Types where
import ImageLoader
import System.Random

data Slider = Slider { 
    ballPos :: Float,
    isSelected :: Bool,
    val :: Int,
    minVal :: Int,
    maxVal :: Int,
    step :: Float
}  deriving Show

data BlackjackGame = Game {
    players :: [Player],
    dealer :: Player,
    gameState :: State,
    images :: Images,
    deck :: Hand,
    randomGen :: StdGen,
    selectedPlayer :: Int,
    slider :: Slider
    -- winner :: Winner
} deriving Show

data Winner = Winner {
    present :: Bool,
    typeOf :: PlayerType,
    winners :: [Player]
} deriving Show

data PlayerType = TypeDealer | TypePlayer deriving Show
data State = BetPhase | Running | GameOver | TakeActionPhase | SessionOver deriving (Eq, Show)
data Player = Player {
    hand :: Hand,
    balance :: Int,
    currentBet :: Int,
    playerPos :: Int,
    finished :: Bool,
    bust :: Bool,
    hasWon :: WinType
} deriving (Show)

data WinType = T | F | Tie | Blackjack | NotDetermined deriving (Show, Eq)

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

