module Types where

data Slider = Slider { 
    ballPos :: Float,
    isSelected :: Bool,
    val :: Int,
    minVal :: Int,
    maxVal :: Int,
    step :: Float
}  deriving Show
