module Handlers where
import State
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace
import MysteriousConstants
import Text.ParserCombinators.ReadPrec (reset)

debug = flip trace

handleInput :: Event -> BlackjackGame -> BlackjackGame
handleInput event state =
    case event of
        EventKey (MouseButton LeftButton) Down _ coords -> handleClick coords state `debug` show state
        _ -> state

handleClick :: (Float, Float) -> BlackjackGame -> BlackjackGame
handleClick coords state =
        case buttonHitted of
            Just Bet -> hittedBetButton state `debug` show (players state !! 0)
            Just Hit -> checkIfBust (hittedHitButton state) -- checkIfBust checks for busted player and/or finished players.
            Just Stand -> hittedStandButton state
            Just NewGame -> hittedNewGameButton state
            _ -> state `debug` show (players state !! 0)
            `debug` show buttonHitted
    where
        buttonHitted = checkButtonHit coords state `debug` show coords

checkIfBust :: BlackjackGame -> BlackjackGame
checkIfBust state = passToDealerIfAllFinished ( state { players = map (\player -> if getNumberByHand (hand player) > 21 then player {finished = True, bust = True} else player) (players state),
                            dealer = if getNumberByHand (hand (dealer state)) > 21 then (dealer state) {finished = True, bust = True} else dealer state
})

passToDealerIfAllFinished :: BlackjackGame -> BlackjackGame
passToDealerIfAllFinished state = if allPlayersFinished then drawCardsForDealer state else state
    where
    allPlayersFinished = all finished (players state)

drawCardsForDealer :: BlackjackGame -> BlackjackGame
drawCardsForDealer state
    | dealerHandSize > 21 = determineWinner (state { dealer = (dealer state) { bust = True, finished = True }, gameState = GameOver })
    | dealerHandSize >= 17 = determineWinner (state { dealer = (dealer state) { finished = True }, gameState = GameOver })
    | otherwise = drawCardsForDealer(state {   dealer = addCardsToDealer (dealer state) topCards,
                            deck = newDeck
                            })
    where
    dealerHandSize = getNumberByHand (hand (dealer state))
    (topCards, newDeck) = getTopCards 1 state ([], deck state)

determineWinner :: BlackjackGame -> BlackjackGame
determineWinner state
    | bust (dealer state) = state { players = -- If dealer is bust, all non bust players win
        map (\player -> if not (bust player) then player {hasWon = T} else player { hasWon = F }) (players state) }
    | not (bust (dealer state)) = state { players =  -- If dealer is not bust, all players with a higher count win
        map (\player ->
            if not (bust player) && getNumberByHand (hand player) > dealerCount then player { hasWon = T }
            else if not (bust player) && getNumberByHand (hand player) == dealerCount then player { hasWon = Tie }
            else player { hasWon = F }
                    ) (players state)
        }
    | otherwise = state -- No winner is present yet (this shouldn't happen)
    where
        dealerCount = getNumberByHand (hand (dealer state))


hittedNewGameButton :: BlackjackGame -> BlackjackGame
hittedNewGameButton state = state { gameState = BetPhase,
            players = map (\player -> player { hand = emptyHand, bust = False, hasWon = NotDetermined,
            finished = False }) (players state),
            dealer = (dealer state) { hand = emptyHand },
            selectedPlayer = 0
             }

hittedStandButton :: BlackjackGame -> BlackjackGame
hittedStandButton state = passToDealerIfAllFinished (state {
    players = map (\player -> if playerPos player == selectedPlayer state then player {finished = True } else player ) (players state)
})

hittedHitButton :: BlackjackGame -> BlackjackGame
hittedHitButton state = state {
    players = addCardsToPlayer (players state) 0 [head topCards],
    deck = newDeck
    }
    where
    (topCards, newDeck) = getTopCards 1 state ([], deck state)

resetBustStatus :: BlackjackGame -> BlackjackGame -- Resets bust status for players and dealer
resetBustStatus state = state { players = map (\player -> player { finished = False, bust = False }) (players state),
                                dealer = (dealer state) { finished = False, bust = False }

}

hittedBetButton :: BlackjackGame -> BlackjackGame
hittedBetButton state = resetBustStatus (state {gameState = TakeActionPhase,
        players = addCardsToPlayer (players state) 0 [head topCards, topCards !! 1], -- Add two cards to player
        dealer = addCardsToDealer (dealer state) [topCards !! 2, topCards !! 3], -- Add cards to dealer
        deck = newDeck
        })
    where
    (topCards, newDeck) = getTopCards 4 state ([], deck state)

addCardsToDealer :: Player -> [Card] -> Player
addCardsToDealer dealer cardsToAdd = dealer { hand = Hand { size = size (hand dealer) + length cardsToAdd ,
                                                            cards = cards (hand dealer) ++ cardsToAdd }}

addCardsToPlayer :: [Player] -- ^ Player list
  -> Int -- ^ Player to modify (player[int])
  -> [Card] -- ^ Cards to add
  -> [Player]
addCardsToPlayer players n cardsToAdd = replaceNth n (player { hand =
                                                                     Hand {
                                                                         size = size (hand player) + length cardsToAdd,
                                                                         cards = cards (hand player) ++ cardsToAdd }
                                                            }) players
    where
        player = players !! n


-- | Get top n cards, return new cards and updated deck
-- | Number of cards to return -> State -> (deckToReturn, deckToDrawFrom)
getTopCards :: Int -> BlackjackGame -> ([Card], Hand) -> ([Card], Hand)
getTopCards n state (deckToReturn, deck)
  | n == 0 = (deckToReturn, deck) -- Finished recursion, return  the new info
  | size deck == 0 = getTopCards n state (deckToReturn, createShuffledDeck state) -- There are no cards to draw, reshuffle and call again
  | otherwise = -- Otherwise add 1 card to the deck to return and call this function again
    getTopCards (n-1) state (deckToReturn ++ [head (cards deck)],
                                Hand { size = size deck - 1, cards = removeFirst (cards deck)})




removeFirst :: [a] -> [a]
removeFirst = \myList ->
    case myList of
        [] -> [] -- if the list is empty, return empty list
        x:xs -> xs -- split head and return remaining list

data Button = Bet | Hit | Stand | NewGame  deriving (Eq, Show)
checkButtonHit :: (Float, Float) -> BlackjackGame -> Maybe Button
checkButtonHit coords state
    | gameState state == BetPhase = if
        and [x > fst (fst betHitbox), x < fst (snd betHitbox),
        y < snd (fst betHitbox), y > snd (snd betHitbox)] then Just Bet
        else Nothing `debug` show [x > fst (fst betHitbox), x < snd (fst betHitbox), y > fst (snd betHitbox), y < snd (snd betHitbox)]
    | gameState state == TakeActionPhase = if
        and [x > fst (fst hitHitbox), x < fst (snd hitHitbox),
        y < snd (fst hitHitbox), y > snd (snd hitHitbox)] then Just Hit
        else if
        and [x > fst (fst standHitbox), x < fst (snd standHitbox),
        y < snd (fst standHitbox), y > snd (snd standHitbox)] then Just Stand
        else Nothing
    | gameState state == GameOver = if
        and [x > fst (fst startNewGameHitbox), x < fst (snd startNewGameHitbox),
        y < snd (fst startNewGameHitbox), y > snd (snd startNewGameHitbox)] then Just NewGame
        else Nothing
    | otherwise = Nothing `debug` "nope2"

    where
        x = fst coords
        y = snd coords
        betHitbox = getHitbox betButtonOffset buttonSize
        hitHitbox = getHitbox hitButtonOffset buttonSize
        standHitbox = getHitbox standButtonOffset buttonSize
        startNewGameHitbox = getHitbox (0,0) buttonSize


-- Get hitbox coords: [Top left, Bottom right]
getHitbox :: (Float, Float) -> (Float, Float) -> ((Float, Float), (Float, Float))
getHitbox offset size = ((x - w/2, y + h/2), (x + w/2, y - h/2))
    where
        x = fst offset
        y = snd offset
        w = fst size
        h = snd size

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs