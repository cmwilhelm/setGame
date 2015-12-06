{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module SetGame.Cards where

import SetGame.Attributes
import Control.Applicative
import Data.List
import Data.SafeCopy
import Data.Aeson (ToJSON(toJSON), object, (.=))


data Card = Card {
  color  :: Color,
  shade  :: Shade,
  number :: Number,
  shape  :: Shape
  } deriving (Eq)

instance Show Card where
  show card = "Card: "
            ++ intercalate ", " (fmap (\acc -> acc card) cardAccessors)

instance ToJSON Card where
   toJSON (Card color shade number shape) =
     object [ "color"  .= show color
            , "shade"  .= show shade
            , "number" .= show number
            , "shape"  .= show shape ]


allCards :: [Card]
allCards = [Card] <*> colors <*> shades <*> numbers <*> shapes

cardAccessors :: [Card -> String]
cardAccessors = [show . color, show . shade, show . number, show . shape]

$(deriveSafeCopy 0 'base ''Card)
