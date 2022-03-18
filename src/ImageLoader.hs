module ImageLoader where
import Graphics.Gloss.Juicy (loadJuicyPNG)
import MysteriousConstants
import Graphics.Gloss.Data.Picture

loadImages :: IO Images
loadImages = do
  -- Just imgBackground <- loadJuicyPNG "img/background.png"
  Just imgButton <- loadJuicyPNG "img/button.png"
  cardImages <- loadCards
  Just imgBetButton <- loadJuicyPNG "img/betButton.png"
  Just imgHitButton <- loadJuicyPNG "img/hitButton.png"
  Just imgStandButton <- loadJuicyPNG "img/standButton.png"
  Just imgDoubleButton <- loadJuicyPNG "img/doubleButton.png"
  return
    Images
      {
      -- background = imgBackground,
        imageButton = imgButton,
        imageCards = cardImages,
        imageBetButton = imgBetButton,
        imageHitButton = imgHitButton,
        imageStandButton = imgStandButton,
        imageDoubleButton = imgDoubleButton
      }

data Images = Images
  { 
    -- background :: Picture,
    imageButton :: Picture,
    imageCards :: [Picture],
    imageBetButton :: Picture,
    imageHitButton :: Picture,
    imageStandButton :: Picture,
    imageDoubleButton :: Picture
  }

loadCards :: IO [Picture]
loadCards = do 
  pictures <- sequence loadedList
  return $ map unwrapPicture pictures
  where
    loadedList = map
      (\x -> loadJuicyPNG $ "img/cards/" ++ x ++ ".png")
      cardPNGNames


unwrapPicture :: Maybe Picture -> Picture
unwrapPicture image = case image of
  Nothing  -> blank
  Just img -> img
