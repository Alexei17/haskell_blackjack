-- | please don't question this file
module MysteriousConstants where

cardPNGNames = ["2C","2D","2H","2S","3C","3D","3H","3S","4C","4D","4H","4S","5C","5D","5H","5S","6C","6D","6H","6S","7C","7D","7H","7S","8C","8D","8H","8S","9C","9D","9H","9S","AC","AD","AH","AS","JC","JD","JH","JS","KC","KD","KH","KS","QC","QD","QH","QS","TC","TD","TH","TS"]

-- Offsets
betButtonOffset, hitButtonOffset, standButtonOffset :: (Float, Float)
betButtonOffset = (0, -300)
hitButtonOffset = (-80, -300)
standButtonOffset = (80, -300)

buttonSize, cardSize :: (Float, Float)
buttonSize = (150, 50)
cardSize = (100, 152)

bufferBetweenCards :: Float
bufferBetweenCards = -60

playersCardsCenter :: [(Float, Float)]
playersCardsCenter = [(0, -410)]
--                      p0
dealerCardsCenter :: (Float, Float)
dealerCardsCenter = (0, 410)