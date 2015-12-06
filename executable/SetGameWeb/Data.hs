{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module SetGameWeb.Data where

import Data.Aeson (ToJSON(toJSON), object, (.=))
import SetGame.Cards


instance ToJSON Card where
   toJSON (Card color shade number shape) = object [ "color"  .= show color
                                                   , "shade"  .= show shade
                                                   , "number" .= show number
                                                   , "shape"  .= show shape ]
