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
    deck = Hand { size = 0, cards = [] },
    selectedPlayer = 0
    }

initInitialPlayer :: Player
initInitialPlayer = Player { hand = emptyHand, balance = 0, currentBet = 0, playerPos = 0, finished = False, bust = False, hasWon = NotDetermined }

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
    -- winner :: Winner
} deriving Show

data Winner = Winner {
    present :: Bool,
    typeOf :: PlayerType,
    winners :: [Player]
} deriving Show

data PlayerType = TypeDealer | TypePlayer deriving Show
data State = BetPhase | Running | GameOver | TakeActionPhase deriving (Eq, Show)
data Player = Player {
    hand :: Hand,
    balance :: Int,
    currentBet :: Int,
    playerPos :: Int,
    finished :: Bool,
    bust :: Bool,
    hasWon :: WinType
} deriving (Show)

data WinType = T | F | Tie | Blackjack | NotDetermined deriving Show

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
