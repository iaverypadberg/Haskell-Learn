module Shapes(
    areaTriangle,
    perimeterTriangle,
    Shape
) where

-- A shape data type which allows for items to be left out
data NewShape = NewShape {
    side1 :: Int,
    side2 :: Int,
    side3 :: Int,
    side4 :: Int
} deriving Show

data Shape = Shape {
    name :: String,
    shape :: NewShape
} deriving Show


makeShape :: Int ->Int -> Int -> Int -> NewShape
makeShape  = NewShape 


areaTriangle ::  Float -> Float -> Float 
areaTriangle b h = h * (1/2 * b)
    
perimeterTriangle :: Int -> Int -> Int -> Int
perimeterTriangle a b c = a + b + c


