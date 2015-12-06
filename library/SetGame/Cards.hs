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
  shape  :: Shape
  } deriving (Eq)

instance Show Card where
  show card = "Card: "
            ++ intercalate ", " (fmap (\acc -> acc card) cardAccessors)

instance Ord Card where
  card1 > card2         = show card1 > show card2
  card1 <= card2        = show card1 <= show card2
  card1 `compare` card2 = (show card1) `compare` (show card2)

instance ToJSON Card where
   toJSON (Card color shade number shape) =
     object [ "color"  .= show color
            , "shade"  .= show shade
            , "number" .= show number
            , "shape"  .= show shape ]


allCards :: [Card]
allCards = [Card] <*> colors <*> shades <*> numbers <*> shapes

allCardsWithIds :: [(Card, Int)]
allCardsWithIds = zip allCards [1..]

cardToIdMap :: Map.Map Card Int
cardToIdMap = Map.fromList allCardsWithIds

idToCardMap :: Map.Map Int Card
idToCardMap = Map.fromList
            . fmap swap
            $ allCardsWithIds

cardAccessors :: [Card -> String]
cardAccessors = [show . color, show . shade, show . number, show . shape]

$(deriveSafeCopy 0 'base ''Card)
