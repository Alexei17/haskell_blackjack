{-# OPTIONS_GHC -Wno-type-defaults #-}
module Handlers where
import State
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace
import MysteriousConstants
import Types

debug :: c -> String -> c
debug = flip trace

handleInput :: Event -> BlackjackGame -> BlackjackGame
handleInput event state =
    case event of
        EventKey (MouseButton LeftButton) Down _ coords -> handleClick coords state -- `debug` show state
        EventKey (MouseButton LeftButton) Up _ _ -> state { 
            slider = (slider state) { isSelected = False }
            }
        EventMotion (x, _) -> if isSelected (slider state)
            then state { slider = moveSliderBall x (slider state) }
            else state
        _ -> state

handleClick :: (Float, Float) -> BlackjackGame -> BlackjackGame
handleClick coords state =
        case buttonHitted of
            Just Bet -> hittedBetButton state -- `debug` show (players state !! 0)
            Just Hit -> checkIfBust (hittedHitButton state) 
            -- checkIfBust checks for busted player and/or finished players.
            Just Stand -> hittedStandButton state
            Just Double -> checkIfBust (hittedDoubleButton state)
            Just NewGame -> hittedNewGameButton state
            Just SliderHit -> hittedSlider state coords
            _ -> state -- `debug` show (players state !! 0)
            -- `debug` show buttonHitted
    where
        buttonHitted = checkButtonHit coords state -- `debug` show coords

checkIfBust :: BlackjackGame -> BlackjackGame
checkIfBust state = passToNextPlayerIfCurrentFinished 
    (passToDealerIfAllFinished 
    (state { players = 
        map  (\player -> if getNumberByHand (hand player) > 21 
            then player {finished = True, bust = True} else player)
            (players state),
            dealer = if getNumberByHand (hand (dealer state)) > 21 
                then (dealer state) {finished = True, bust = True} else dealer state
}))

passToNextPlayerIfCurrentFinished :: BlackjackGame -> BlackjackGame
passToNextPlayerIfCurrentFinished state = if finished (players state !! selectedPlayer state)
                                    then state { selectedPlayer = 
                                        mod (selectedPlayer state + 1) (length (players state)) }
                                    else state

passToDealerIfAllFinished :: BlackjackGame -> BlackjackGame
passToDealerIfAllFinished state = if allPlayersFinished then drawCardsForDealer state else state
    where
    allPlayersFinished = all finished (players state)

drawCardsForDealer :: BlackjackGame -> BlackjackGame
drawCardsForDealer state
    | dealerHandSize > 21 = determineWinner 
    (state { dealer = (dealer state) { bust = True, finished = True }, gameState = GameOver })
    | dealerHandSize >= 17 = determineWinner 
    (state { dealer = (dealer state) { finished = True }, gameState = GameOver })
    | otherwise = drawCardsForDealer(state {   dealer = addCardsToDealer (dealer state) topCards,
                            deck = newDeck
                            })
    where
    dealerHandSize = getNumberByHand (hand (dealer state))
    (topCards, newDeck) = getTopCards 1 state ([], deck state)

determineWinner :: BlackjackGame -> BlackjackGame
determineWinner state
    | bust (dealer state) = handeEndGameBets (state { players = -- If dealer is bust, all non bust players win
        map (\player -> if not (bust player) then player {hasWon = T} 
        else player { hasWon = F }) (players state) })
    | not (bust (dealer state)) = handeEndGameBets (state { players =  -- If dealer is not bust, all players with a higher count win
        map (\player ->
            if not (bust player) && getNumberByHand (hand player) == 21 && size (hand player) == 2 
                then player { hasWon = Blackjack }
            else if not (bust player) && getNumberByHand (hand player) > dealerCount 
                then player { hasWon = T }
            else if not (bust player) && getNumberByHand (hand player) == dealerCount 
                then player { hasWon = Tie }
            else player { hasWon = F }
                    ) (players state)
        })
    | otherwise = state -- No winner is present yet (this shouldn't happen)
    where
        dealerCount = getNumberByHand (hand (dealer state))

handeEndGameBets :: BlackjackGame -> BlackjackGame
handeEndGameBets state = state { players = map (\player ->
    if hasWon player == T 
        then player { balance = balance player - currentBet player + currentBet player * 2 }
    else if hasWon player == F 
        then player { balance = balance player - currentBet player }
    else if hasWon player == Blackjack 
        then player { balance = balance player - currentBet player + currentBet player * 3 }
    else player) (players state)}

hittedNewGameButton :: BlackjackGame -> BlackjackGame
hittedNewGameButton state =  updateSliderData ( checkEndGame (state { gameState = BetPhase,
            players = map (\player -> player { hand = emptyHand, bust = False, hasWon = NotDetermined,
            finished = False }) (players state),
            dealer = (dealer state) { hand = emptyHand },
            selectedPlayer = 0
            }))


checkEndGame :: BlackjackGame -> BlackjackGame
checkEndGame state = state {players = map (\player -> if balance player <= 0 
    then player { balance = 1000} 
    else player) (players state)}

hittedDoubleButton :: BlackjackGame -> BlackjackGame -- Doubling is the same as hitting and standing, but double the balance.
hittedDoubleButton state = hittedStandButton ( state {
    players = addCardsToPlayer (players state) (selectedPlayer state) [head topCards] True,
    deck = newDeck
    })
    where
    (topCards, newDeck) = getTopCards 1 state ([], deck state)

hittedStandButton :: BlackjackGame -> BlackjackGame
hittedStandButton state = passToNextPlayerIfCurrentFinished (passToDealerIfAllFinished (state {
    players = map (\player -> if playerPos player == selectedPlayer state 
        then player {finished = True } else player ) (players state)
}))

hittedHitButton :: BlackjackGame -> BlackjackGame
hittedHitButton state = state {
    players = addCardsToPlayer (players state) (selectedPlayer state) [head topCards] False,
    deck = newDeck
    }
    where
    (topCards, newDeck) = getTopCards 1 state ([], deck state)

resetBustStatus :: BlackjackGame -> BlackjackGame -- Resets bust status for players and dealer
resetBustStatus state = state { players = map 
    (\player -> player { finished = False, bust = False }) (players state),
    dealer = (dealer state) { finished = False, bust = False }
}

sliderDataToBet :: BlackjackGame -> BlackjackGame
sliderDataToBet state = state { players = map (\player -> if playerPos player == selectedPlayer state
   then player { currentBet = val $ slider state} else player) (players state) }

hittedBetButton :: BlackjackGame -> BlackjackGame
hittedBetButton state = updateSliderData 
                        (sliderDataToBet 
                        (resetBustStatus 
                        (changeGameStateOrShift TakeActionPhase 
                        (state {
    players = addCardsToPlayer (players state) (selectedPlayer state) [head topCards, topCards !! 1] False, -- Add two cards to player
    dealer = if selectedPlayer state == 0 
        then addCardsToDealer (dealer state) [topCards !! 2, topCards !! 3] 
        else dealer state, -- Add cards to dealer if it's first players turn
        deck = newDeck
        }))))
    where
    (topCards, newDeck) = if selectedPlayer state == 0 
                        then getTopCards 4 state ([], deck state) 
                        else getTopCards 2 state ([], deck state)

changeGameStateOrShift :: State -> BlackjackGame -> BlackjackGame
changeGameStateOrShift gStateTo state
    | selectedPlayer state == length (players state) - 1 = state 
    { gameState = gStateTo, selectedPlayer = 0 } -- All players took their turn, can convert to next state
    | otherwise = state { selectedPlayer = selectedPlayer state + 1 } -- Pass on to next player

addCardsToDealer :: Player -> [Card] -> Player
addCardsToDealer dealer_ cardsToAdd = dealer_ { hand = Hand { 
    size = size (hand dealer_) + length cardsToAdd ,
    cards = cards (hand dealer_) ++ cardsToAdd }}

addCardsToPlayer :: [Player] -- ^ Player list
  -> Int -- ^ Player to modify (player[int])
  -> [Card] -- ^ Cards to add
  -> Bool -- ^ came from double button
  -> [Player]
addCardsToPlayer players_ n cardsToAdd doubleButton = replaceNth n (player { hand =
                                                                     Hand {
size = size (hand player) + length cardsToAdd,
cards = cards (hand player) ++ cardsToAdd },
currentBet = if doubleButton then currentBet player * 2 else currentBet player
                                                            }) players_
    where
        player = players_ !! n


-- | Get top n cards, return new cards and updated deck
-- | Number of cards to return -> State -> (deckToReturn, deckToDrawFrom)
getTopCards :: Int -> BlackjackGame -> ([Card], Hand) -> ([Card], Hand)
getTopCards n state (deckToReturn, deck_)
  | n == 0 = (deckToReturn, deck_) -- Finished recursion, return  the new info
  | size deck_ == 0 = getTopCards n state (deckToReturn, createShuffledDeck state) -- There are no cards to draw, reshuffle and call again
  | otherwise = -- Otherwise add 1 card to the deck to return and call this function again
    getTopCards (n-1) state (deckToReturn ++ [head (cards deck_)],
                                Hand { size = size deck_ - 1, cards = removeFirst (cards deck_)})




removeFirst :: [a] -> [a]
removeFirst = \myList ->
    case myList of
        [] -> [] -- if the list is empty, return empty list
        _:xs -> xs -- split head and return remaining list

data Button = Bet | Hit | Stand | NewGame | SliderHit | Double deriving (Eq, Show)
checkButtonHit :: (Float, Float) -> BlackjackGame -> Maybe Button
checkButtonHit coords state
    | gameState state == BetPhase = if
        and [x > fst (fst betHitbox), x < fst (snd betHitbox),
        y < snd (fst betHitbox), y > snd (snd betHitbox)] then Just Bet
        else if and [x > fst (fst sliderHitbox), x < fst (snd sliderHitbox),
        y < snd (fst sliderHitbox), y > snd (snd sliderHitbox)] then Just SliderHit
        else Nothing
    | gameState state == TakeActionPhase = if
        and [x > fst (fst hitHitbox), x < fst (snd hitHitbox),
        y < snd (fst hitHitbox), y > snd (snd hitHitbox)] then Just Hit
        else if
        and [x > fst (fst standHitbox), x < fst (snd standHitbox),
        y < snd (fst standHitbox), y > snd (snd standHitbox)] then Just Stand
        else if
        and [x > fst (fst doubleHitbox), x < fst (snd doubleHitbox),
        y < snd (fst doubleHitbox), y > snd (snd doubleHitbox)] then Just Double
        else Nothing
    | gameState state == GameOver = if
        and [x > fst (fst startNewGameHitbox), x < fst (snd startNewGameHitbox),
        y < snd (fst startNewGameHitbox), y > snd (snd startNewGameHitbox)] then Just NewGame
        else Nothing
    | otherwise = Nothing

    where
        x = fst coords
        y = snd coords
        betHitbox = getHitbox betButtonOffset buttonSize
        hitHitbox = getHitbox hitButtonOffset buttonSize
        standHitbox = getHitbox standButtonOffset buttonSize
        startNewGameHitbox = getHitbox (0,0) buttonSize
        doubleHitbox = getHitbox doubleButtonOffset buttonSize
        sliderHitbox = getHitbox sliderOffset sliderSize


-- Get hitbox coords: [Top left, Bottom right]
getHitbox :: (Float, Float) -> (Float, Float) -> ((Float, Float), (Float, Float))
getHitbox offset size_ = ((x - w/2, y + h/2), (x + w/2, y - h/2))
    where
        x = fst offset
        y = snd offset
        w = fst size_
        h = snd size_

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

updateSliderData :: BlackjackGame -> BlackjackGame
updateSliderData state = state {
    slider = (slider state) {
        maxVal = balance $ players state !! selectedPlayer state,
        minVal = 0,
        val = 0,
        step = if r == 0
            then sliderWidth
            else sliderWidth / (r / 5),
        ballPos = -150,
        isSelected = False
        }}
        where
            r = fromIntegral $ balance (players state !! selectedPlayer state)
            sliderWidth = fst sliderSize - 2 * fst sliderSize


hittedSlider :: BlackjackGame -> (Float,Float) -> BlackjackGame
hittedSlider state pos = state {
    slider = handleSliderClick (fst pos) (slider state)
    }

handleSliderClick :: Float -> Slider -> Slider
handleSliderClick x slider_ = slider_
    { isSelected   = True
    , ballPos = -150 + x2
    , val = bet
    }
  where
    x2 = x + fromIntegral (round sliderHalfWidth)
    bet = if x2 >= sliderHalfWidth * 2
      then maxVal slider_
      else minVal slider_ + abs (floor (x2 / step slider_) * 5)
    sliderHalfWidth  = fst sliderSize / 2

moveSliderBall :: Float -> Slider -> Slider
moveSliderBall x = handleSliderClick newX
  where
    sliderHitBox = getHitbox sliderOffset sliderSize
    minX = fst $ fst sliderHitBox
    maxX = fst $ snd sliderHitBox
    newX
      | x < minX = minX
      | x > maxX = maxX
      | otherwise = x