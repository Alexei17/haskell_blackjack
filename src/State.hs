module State where
import ImageLoader


initGameState :: IO BlackjackGame
initGameState = createGameStateWith <$> loadImages

createGameStateWith :: Images -> BlackjackGame
createGameStateWith imgs = Game { 
    players = [initInitialPlayer], 
    dealer = initInitialPlayer, 
    gameState = Running,
    images = imgs
    }

initInitialPlayer :: Player
initInitialPlayer = Player { hand = emptyHand, balance = 0, currentBet = 0 }

emptyHand :: Hand
emptyHand = Hand { cards = Nothing, size = 0 }

data BlackjackGame = Game {
    players :: [Player],
    dealer :: Player,
    gameState :: State,
    images :: Images
}

data State = Running | GameOver

data Player = Player {
    hand :: Hand,
    balance :: Int,
    currentBet :: Int
}

data Hand = Hand {
    cards :: Maybe [Card],
    size :: Int
}

data Card = Card {
    cardRank :: CardRank,
    cardSuit :: CardSuit
}

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

data CardSuit
    = Clubs
    | Spades
    | Diamonds
    | Hearts

