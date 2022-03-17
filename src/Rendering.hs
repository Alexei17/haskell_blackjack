module Rendering where
import Graphics.Gloss
import State
import Offsets
import ImageLoader

-- gameAsPicture = pictures []
drawScreen :: BlackjackGame -> Picture
drawScreen state = drawButtons state

drawButtons :: BlackjackGame -> Picture
drawButtons state = translate 0 0 ((imageCards $ images state) !! 0)

-- drawScreen state = pictures [imageButton $ images state]