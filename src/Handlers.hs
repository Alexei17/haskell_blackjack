module Handlers where
import State
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace
import Offsets

debug = flip trace

handleInput :: Event -> BlackjackGame -> BlackjackGame
handleInput event state =
    case event of
        EventKey (MouseButton LeftButton) Down _ coords -> handleClick coords state
        _ -> state

handleClick :: (Float, Float) -> BlackjackGame -> BlackjackGame
handleClick coords state = 
        case buttonHitted of
            Just Bet -> state {gameState = TakeActionPhase}
            _ -> state 
            `debug` show buttonHitted
    where
        buttonHitted = checkButtonHit coords state `debug` show coords

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