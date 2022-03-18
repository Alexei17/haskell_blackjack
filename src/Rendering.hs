module Rendering where
import Graphics.Gloss
import State
import Offsets
import ImageLoader

-- gameAsPicture = pictures []
drawScreen :: BlackjackGame -> Picture
drawScreen state 
    | gameState state == BetPhase = pictures (drawButtons state)
    | gameState state == TakeActionPhase = pictures (drawButtons state)
    | otherwise = pictures [drawCards state]

drawCards :: BlackjackGame -> Picture
drawCards state = translate 0 0 ((imageCards $ images state) !! 0)

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

-- drawScreen state = pictures [imageButton $ images state]