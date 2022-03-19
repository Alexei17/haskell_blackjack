module Handlers where
import State
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace
import MysteriousConstants

debug = flip trace

handleInput :: Event -> BlackjackGame -> BlackjackGame
handleInput event state =
    case event of
        EventKey (MouseButton LeftButton) Down _ coords -> handleClick coords state
        _ -> state

handleClick :: (Float, Float) -> BlackjackGame -> BlackjackGame
handleClick coords state =
        case buttonHitted of
            Just Bet -> hittedBetButton state `debug` show (players state !! 0)
            Just Hit -> hittedHitButton state
            _ -> state `debug` show (players state !! 0)
            `debug` show buttonHitted
    where
        buttonHitted = checkButtonHit coords state `debug` show coords


hittedHitButton :: BlackjackGame -> BlackjackGame
hittedHitButton state = state { 
    players = addCardsToPlayer (players state) 0 [head topCards],
    deck = newDeck
    }
    where
    (topCards, newDeck) = getTopCards 1 state ([], deck state)

hittedBetButton :: BlackjackGame -> BlackjackGame
hittedBetButton state = state {gameState = TakeActionPhase,
        players = addCardsToPlayer (players state) 0 [head topCards, topCards !! 1], -- Add two cards to player
        dealer = addCardsToDealer (dealer state) [topCards !! 2, topCards !! 3], -- Add cards to dealer
        deck = newDeck
        }
    where
    (topCards, newDeck) = getTopCards 4 state ([], deck state)

addCardsToDealer :: Player -> [Card] -> Player
addCardsToDealer player cardsToAdd = player { hand = Hand { size = size (hand player) + length cardsToAdd ,
                                                            cards = cards (hand player) ++ cardsToAdd }}

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

data Button = Bet | Hit | Stand  deriving (Eq, Show)
checkButtonHit :: (Float, Float) -> BlackjackGame -> Maybe Button
checkButtonHit coords state
    | gameState state == BetPhase = if
        and [x > fst (fst betHitbox), x < fst (snd betHitbox),
        y < snd (fst betHitbox), y > snd (snd betHitbox)] then Just Bet
        else Nothing `debug` show [x > fst (fst betHitbox), x < snd (fst betHitbox), y > fst (snd betHitbox), y < snd (snd betHitbox)]
    | gameState state == TakeActionPhase = if
        and [x > fst (fst hitHitbox), x < fst (snd hitHitbox),
        y < snd (fst hitHitbox), y > snd (snd hitHitbox)] then Just Hit else if
        and [x > fst (fst standHitbox), x < fst (snd standHitbox),
        y < snd (fst standHitbox), y > snd (snd standHitbox)] then Just Stand
        else Nothing


    | otherwise = Nothing `debug` "nope2"

    where
        x = fst coords
        y = snd coords
        betHitbox = getHitbox betButtonOffset buttonSize
        hitHitbox = getHitbox hitButtonOffset buttonSize
        standHitbox = getHitbox standButtonOffset buttonSize

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