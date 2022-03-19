module Rendering where
import Graphics.Gloss
import State
import MysteriousConstants
import ImageLoader

-- gameAsPicture = pictures []
drawScreen :: BlackjackGame -> Picture
drawScreen state
    | gameState state == BetPhase = pictures (drawButtons state)
    | gameState state == TakeActionPhase = pictures ((drawButtons state) ++ (drawCards state))
    | otherwise = pictures (drawCards state)

-- drawCards :: BlackjackGame -> Picture
-- drawCards state = translate 0 0 ((imageCards $ images state) !! 0)

drawButton :: BlackjackGame -> Picture
drawButton state = uncurry translate betButtonOffset (imageButton $ images state)

drawButtons :: BlackjackGame -> [Picture]
drawButtons state
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
drawCards state = concatMap (\player -> drawCardsForPlayer player (imageCards (images state))) (players state)

drawCardsForPlayer :: Player -> [Picture] -> [Picture]
drawCardsForPlayer player imageCards = map (\(card,xPos) -> drawSingleCard card xPos cardsAreaY imageCards) zippedList
    where
        cardsAreaCenter = playersCardsCenter !! (playerPos player)
        cardsAmount = size (hand player)
        cardWidth = fst cardSize
        cardsAreaWidth = fromIntegral cardsAmount * cardWidth + (fromIntegral cardsAmount - 1) * bufferBetweenCards
        cardsOffsetFromLeft = [(cardWidth / 2),(cardWidth / 2) + cardWidth + (bufferBetweenCards)..cardsAreaWidth-50]
        card_xAxis_CenteresAt0 = map (subtract (cardWidth + (bufferBetweenCards / 2))) cardsOffsetFromLeft
        cardxAxisCenters = map (+ (fst cardsAreaCenter - ((fromIntegral cardsAmount - 2) * (cardWidth / 2 + bufferBetweenCards / 2)))) card_xAxis_CenteresAt0
        zippedList = zipWith (\x y -> (x,y)) (cards (hand player)) cardxAxisCenters
        -- ^ zippedList = [(Card, Float)] aka [(Card, xPos)]
        cardsAreaY = snd cardsAreaCenter

drawSingleCard :: Card -> Float -> Float -> [Picture] -> Picture
drawSingleCard card x y imageCards = translate x y (imageCards !! (fromEnum $ card))
-- drawScreen state = pictures [imageButton $ images state]