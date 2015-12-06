{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module SetGame.Cards where

import           SetGame.Attributes
import           Control.Applicative
import           Data.List
import qualified Data.Map as Map
import           Data.SafeCopy
import           Data.Tuple
import           Data.Aeson (ToJSON(toJSON), object, (.=))


data Card = Card {
  color  :: Color,
  shade  :: Shade,
  number :: Number,
  shape  :: Shape,
  cardId :: Int
  } deriving (Eq)

instance Show Card where
  show card = "Card: "
            ++ intercalate ", " (fmap (\acc -> acc card) cardAccessors)

instance ToJSON Card where
   toJSON (Card color shade number shape cardId) =
     object [ "color"  .= show color
            , "shade"  .= show shade
            , "number" .= show number
            , "shape"  .= show shape
            , "cardId" .= cardId ]


allCards :: [Card]
allCards = addIds $ [Card] <*> colors <*> shades <*> numbers <*> shapes
  where addIds partials = zipWith ($) partials [1..]

idToCardMap :: Map.Map Int Card
idToCardMap = Map.fromList makeListForCards
  where makeListForCards = fmap (\card -> (cardId card, card)) allCards

cardAccessors :: [Card -> String]
cardAccessors = [show . color, show . shade, show . number, show . shape]

$(deriveSafeCopy 0 'base ''Card)
