module Rendering where
import Graphics.Gloss
import State
import MysteriousConstants
import ImageLoader
import Data.Maybe

-- gameAsPicture = pictures []
drawScreen :: BlackjackGame -> Picture
drawScreen state
    | gameState state == BetPhase = pictures (drawButtons state)
    | gameState state == TakeActionPhase = pictures (drawButtons state ++ drawCards state)
    | gameState state == GameOver = pictures (drawCards state ++ drawEndGameStatus state )
    | otherwise = pictures (drawCards state)


drawEndGameStatus :: BlackjackGame -> [Picture]
drawEndGameStatus state = drawWonAndLostPerPlayer state ++ drawBusts state ++ drawStartNewGameButton state

drawStartNewGameButton :: BlackjackGame -> [Picture]
drawStartNewGameButton state = [translate 0 0 (imagestartNewGame $ images state)]

drawWonAndLostPerPlayer :: BlackjackGame -> [Picture]
drawWonAndLostPerPlayer state = map
    (\player -> uncurry translate (playersEndgameDrawCenter !! playerPos player) (imageToDrawByWinType state player)) (players state)

drawBusts :: BlackjackGame -> [Picture]
drawBusts state = mapMaybe (\player ->
    if bust player then
        Just (uncurry translate (playersEndgameBustsDrawCenter !! playerPos player) (imageBust $ images state))
    else Nothing)
    (players state) ++ (if bust (dealer state) then
        [uncurry translate dealerEndgameBustsDrawCenter (imageBust $ images state)] else [])

imageToDrawByWinType :: BlackjackGame -> Player -> Picture
imageToDrawByWinType state player = case hasWon player of
        T -> imageYouWon $ images state
        F -> imageYouLost $ images state
        Tie -> imagePushed $ images state
        Blackjack -> imageYouWon $ images state
        _ -> imageYouLost $ images state

drawButton :: BlackjackGame -> Picture
drawButton state = uncurry translate betButtonOffset (imageButton $ images state)

drawButtons :: BlackjackGame -- ^ State
--   -> Int -- ^ Current selected player
  -> [Picture]
drawButtons state --n
    | gameState state == BetPhase =
        [
            uncurry translate betButtonOffset (imageBetButton $ images state)
        ]
    | gameState state == TakeActionPhase =
        [
            uncurry translate hitButtonOffset (imageHitButton $ images state),
            uncurry translate standButtonOffset (imageStandButton $ images state)
        ]
    | otherwise = [uncurry translate betButtonOffset (imageButton $ images state)]

drawCards :: BlackjackGame -> [Picture]
drawCards state = drawCardsForDealer (dealer state) (imageCards (images state)) (images state) shouldHideCard ++ concatMap (\player -> drawCardsForPlayer player (imageCards (images state))) (players state)
    where
    shouldHideCard = gameState state == TakeActionPhase

drawCardsForPlayer :: Player -> [Picture] -> [Picture]
drawCardsForPlayer player imageCards = drawPlayerCardCount player : map (\(card,xPos) -> drawSingleCard card xPos cardsAreaY imageCards) zippedList
    where
        cardsAreaCenter = playersCardsCenter !! playerPos player
        cardsAmount = size (hand player)
        cardWidth = fst cardSize
        cardsAreaWidth = fromIntegral cardsAmount * cardWidth + (fromIntegral cardsAmount - 1) * bufferBetweenCards
        cardsOffsetFromLeft = [(cardWidth / 2),(cardWidth / 2) + cardWidth + (bufferBetweenCards)..cardsAreaWidth-50]
        card_xAxis_CenteresAt0 = map (subtract (cardWidth + (bufferBetweenCards / 2))) cardsOffsetFromLeft
        cardxAxisCenters = map (+ (fst cardsAreaCenter - ((fromIntegral cardsAmount - 2) * (cardWidth / 2 + bufferBetweenCards / 2)))) card_xAxis_CenteresAt0
        zippedList = zipWith (\x y -> (x,y)) (cards (hand player)) cardxAxisCenters
        -- ^ zippedList = [(Card, Float)] aka [(Card, xPos)]
        cardsAreaY = snd cardsAreaCenter

drawDealerCardCount :: Player -> Picture
drawDealerCardCount player = uncurry translate dealerCardsNumberCenter number
    where
        number = drawSmallText white (show (getNumberByHand (hand player)))

drawPlayerCardCount :: Player -> Picture
drawPlayerCardCount player =  uncurry translate (playersCardsNumberCenter !! playerPos player) number
    where
        number = drawSmallText white (show (getNumberByHand (hand player)))

drawCardsForDealer :: Player -> [Picture] -> Images -> Bool -> [Picture]
drawCardsForDealer player imageCards images hideOneCard =
    if hideOneCard
        then drawSingleCard (fst (zippedList !! 0)) (snd (zippedList !! 0)) cardsAreaY imageCards :
        [translate (snd (zippedList !! 1)) cardsAreaY (imageBackCard images)] -- Draw one hidden card.
    else drawDealerCardCount player : map (\(card,xPos) -> drawSingleCard card xPos cardsAreaY imageCards) zippedList
    where
        cardsAreaCenter = dealerCardsCenter
        cardsAmount = size (hand player)
        cardWidth = fst cardSize
        cardsAreaWidth = fromIntegral cardsAmount * cardWidth + (fromIntegral cardsAmount - 1) * bufferBetweenCards
        cardsOffsetFromLeft = [(cardWidth / 2),(cardWidth / 2) + cardWidth + (bufferBetweenCards)..cardsAreaWidth-50]
        card_xAxis_CenteresAt0 = map (subtract (cardWidth + (bufferBetweenCards / 2))) cardsOffsetFromLeft
        cardxAxisCenters = map (+ (fst cardsAreaCenter - ((fromIntegral cardsAmount - 2) * (cardWidth / 2 + bufferBetweenCards / 2)))) card_xAxis_CenteresAt0
        zippedList = zipWith (\x y -> (x,y)) (cards (hand player)) cardxAxisCenters
        -- ^ zippedList = [(Card, Float)] aka [(Card, xPos)]
        cardsAreaY = snd cardsAreaCenter

drawSingleCard :: Card -> Float -> Float -> [Picture]  -> Picture
drawSingleCard card x y imageCards = translate x y (imageCards !! (fromEnum $ card))
-- drawSingleCard _ x y _ shouldHideCard = translate x y imageBackCard
-- drawScreen state = pictures [imageButton $ images state]

drawSmallText :: Color -> String -> Picture
drawSmallText clr string =  color clr $ scale 0.25 0.25 (text string)