{-# LANGUAGE TemplateHaskell    #-}
module SetGame.Attributes where

import Data.SafeCopy


data Color = Blue
           | Green
           | Purple deriving (Show, Eq)

data Shade = Solid
           | Striped
           | Empty deriving (Show, Eq)

data Number = One
            | Two
            | Three deriving (Show, Eq)

data Shape = Oval
           | Rectangle
           | Squiggle deriving (Show, Eq)


$(deriveSafeCopy 0 'base ''Color)
$(deriveSafeCopy 0 'base ''Shade)
$(deriveSafeCopy 0 'base ''Number)
$(deriveSafeCopy 0 'base ''Shape)


type AttributeSet a = [a]

colors :: AttributeSet Color
colors = [Blue, Green, Purple]

shades :: AttributeSet Shade
shades = [Solid, Striped, Empty]

numbers :: AttributeSet Number
numbers = [One, Two, Three]

shapes :: AttributeSet Shape
shapes = [Oval, Rectangle, Squiggle]
