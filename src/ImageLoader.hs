module ImageLoader where
import Graphics.Gloss.Juicy (loadJuicyPNG)
import MysteriousConstants
import Graphics.Gloss.Data.Picture
import Data.Maybe

loadImages :: IO Images
loadImages = do
  -- Just imgBackground <- loadJuicyPNG "img/background.png"
  imgButton <- loadJuicyPNG "img/button.png"
  cardImages <- loadCards
  imgBetButton <- loadJuicyPNG "img/betButton.png"
  imgHitButton <- loadJuicyPNG "img/hitButton.png"
  imgStandButton <- loadJuicyPNG "img/standButton.png"
  imgDoubleButton <- loadJuicyPNG "img/doubleButton.png"
  imgBust <- loadJuicyPNG "img/bust.png"
  imgYouWon <- loadJuicyPNG "img/youWon.png"
  imgYouLost <- loadJuicyPNG "img/youLost.png"
  imgStartNewGame <- loadJuicyPNG "img/startNewGame.png"
  imgPushed <- loadJuicyPNG "img/pushed.png"
  return
    Images
      {
      -- background = imgBackground,
        imageButton = fromMaybe Blank imgButton,
        imageCards = cardImages,
        imageBetButton = fromMaybe Blank imgBetButton,
        imageHitButton = fromMaybe Blank imgHitButton,
        imageStandButton = fromMaybe Blank imgStandButton,
        imageDoubleButton = fromMaybe Blank imgDoubleButton,
        imageBust = fromMaybe Blank imgBust,
        imageYouWon = fromMaybe Blank imgYouWon,
        imageYouLost = fromMaybe Blank imgYouLost,
        imagestartNewGame = fromMaybe Blank imgStartNewGame,
        imagePushed = fromMaybe Blank imgPushed
      }

data Images = Images
  {
    -- background :: Picture,
    imageButton :: Picture,
    imageCards :: [Picture],
    imageBetButton :: Picture,
    imageHitButton :: Picture,
    imageStandButton :: Picture,
    imageDoubleButton :: Picture,
    imageBust :: Picture,
    imageYouWon :: Picture,
    imageYouLost :: Picture,
    imagestartNewGame :: Picture,
    imagePushed :: Picture
  } deriving Show

loadCards :: IO [Picture]
loadCards = do
  pictures <- sequence loadedList
  return $ map (fromMaybe Blank) pictures
  where
    loadedList = map
      (\x -> loadJuicyPNG $ "img/cards/" ++ x ++ ".png")
      cardPNGNames
