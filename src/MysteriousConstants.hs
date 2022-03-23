-- | please don't question this file
module MysteriousConstants where

cardPNGNames :: [[Char]]
cardPNGNames = ["2C","2D","2H","2S","3C","3D","3H","3S","4C","4D","4H","4S","5C","5D","5H","5S","6C","6D","6H","6S","7C","7D","7H","7S","8C","8D","8H","8S","9C","9D","9H","9S","AC","AD","AH","AS","JC","JD","JH","JS","KC","KD","KH","KS","QC","QD","QH","QS","TC","TD","TH","TS"]
errorMessage :: [Char]
errorMessage = "You didn't enter a number or entered an invalid number of players. Acceptable number of players is 1,2 or 3."

-- Offsets
betButtonOffset, hitButtonOffset, standButtonOffset, doubleButtonOffset :: (Float, Float)
betButtonOffset = (0, -250)
hitButtonOffset = (-80, -250)
standButtonOffset = (80, -250)
doubleButtonOffset = (0, -195)

buttonSize, cardSize :: (Float, Float)
buttonSize = (150, 50)
cardSize = (100, 152)

bufferBetweenCards :: Float
bufferBetweenCards = -60

playersCardsCenter, playersCardsNumberCenter, playersEndgameDrawCenter, playersEndgameBustsDrawCenter :: [(Float, Float)]
playersCardsCenter =        [(0, -360), (-250, -60), (250, -60)]
playersCardsNumberCenter =  [(-20, -480), (-270, -180), (270, -180)]
playersEndgameDrawCenter =  [(0, -250), (-250, 50), (250, 50)]
playersEndgameBustsDrawCenter = [(0, -200), (-250, 100), (250, 100)]
--                              p0              p1          p2
dealerCardsCenter, dealerCardsNumberCenter, dealerEndgameBustsDrawCenter :: (Float, Float)
dealerCardsCenter = (0, 410)
dealerCardsNumberCenter = (-20, 230)
dealerEndgameBustsDrawCenter = (0, 300)

sliderOffset, sliderSize, sliderTextOffset :: (Float, Float)
sliderOffset = (0,0)
sliderSize = (300, 15)
sliderTextOffset = (-20, -50)