module Cards where

import Attributes
import Control.Applicative
import Data.List


data Card = Card {
  color  :: Color,
  shade  :: Shade,
  number :: Number,
  shape  :: Shape
  } deriving (Eq)

instance Show Card where
  show card = "Card: "
            ++ intercalate ", " (fmap (\acc -> acc card) cardAccessors)

allCards :: [Card]
allCards = [Card] <*> colors <*> shades <*> numbers <*> shapes

cardAccessors :: [Card -> String]
cardAccessors = [show . color, show . shade, show . number, show . shape]
