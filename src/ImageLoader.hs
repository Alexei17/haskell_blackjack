module ImageLoader where
import Graphics.Gloss.Juicy (loadJuicyPNG)
import MysteriousConstants
import Graphics.Gloss.Data.Picture

loadImages :: IO Images
loadImages = do
  -- Just imgBackground <- loadJuicyPNG "img/background.png"
  Just imgButton <- loadJuicyPNG "img/button.png"
  cardImages <- loadCards
  return
    Images
      {
      -- background = imgBackground,
        imageButton = imgButton,
        imageCards = cardImages

      }

data Images = Images
  { 
    -- background :: Picture,
    imageButton :: Picture,
    imageCards :: [Picture]
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
