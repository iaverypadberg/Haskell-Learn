
data CarStuff = CarStuff{
    miles :: Int,
    name :: String,
    carType :: String
} deriving(Show)

data Card = Card{
    mile :: Int,
    nam :: String,
    carTyp :: String
} deriving(Show)

newtype Honda = Honda CarStuff deriving(Show)
newtype Toyota = Toyota CarStuff deriving(Show)


class Person a where
    drive :: a -> String
    crash :: a -> String

instance Person Honda where
    drive (Honda a) = "Driving"
    crash (Honda a) = "Crashed!"

instance Person Toyota where
    drive (Toyota a) = "Driving"
    crash (Toyota a) = "Crashedd!"

instance Person Card where
    drive a = "Driving"
    crash a = "Crashedd!"

