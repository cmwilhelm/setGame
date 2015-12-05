module SetGame.Attributes where


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

type AttributeSet a = [a]


colors :: AttributeSet Color
colors = [Blue, Green, Purple]

shades :: AttributeSet Shade
shades = [Solid, Striped, Empty]

numbers :: AttributeSet Number
numbers = [One, Two, Three]

shapes :: AttributeSet Shape
shapes = [Oval, Rectangle, Squiggle]
