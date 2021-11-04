import Data.Complex (Complex ((:+)))
import Data.Ratio

data Coder = Coder
  { name :: String,
    age :: Double,
    salary :: Int
  }

main :: IO ()
main = do
  print $ Coder "HOla" 2.1 20

instance Show Coder where
  show c =
    "("
      ++ show (name c)
      ++ "/"
      ++ show (age c)
      ++ "/"
      ++ show (salary c)
      ++ ")"
