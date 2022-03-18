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
    images = imgs
    -- deck = createShuffledDeck
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
    deck :: Hand
}

data State = BetPhase | Running | GameOver | TakeActionPhase deriving (Eq)
data Player = Player {
    hand :: Hand,
    balance :: Int,
    currentBet :: Int
}

data Hand = Hand {
    size :: Int,
    cards :: [Card]
}

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


shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))


createShuffledDeck :: Hand
createShuffledDeck = Hand {
    size = 52,
    cards = Card <$> cardRanks <*> cardSuits
}
