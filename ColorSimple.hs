module ColorSimple (Color) where

data Color = RGB
  { red :: Int,
    blue :: Int,
    green :: Int
  }
  deriving (Show)
