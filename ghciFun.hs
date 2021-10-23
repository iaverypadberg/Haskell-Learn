
module Fun (testWhere) where
import Data.Char
import GHC.Integer

removeNonUpper :: String -> String
removeNonUpper xs = [ c | c <- xs, c `elem`['A'..'Z']]

-- map toUpper String
-- case is basically the same thing as pattern matching
stringToUpper :: String -> String
stringToUpper xs =
    case xs of 
        [] -> []
        (x : xs) -> toUpper x : stringToUpper xs

-- map toLower String
stringToLower :: String -> String
stringToLower xs = 
    case xs of
        [] -> []
        ( x : xs) -> toLower x : stringToLower xs

-- Case expression used in the middle of a larger expression
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 

-- Factorial pattern matching
-- Always have the most specific patterns first
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

-- Basic head impimentation
-- Polymorphic because its a list of anything
head' :: [a] -> a
head' [] = error "Empty List"
head' (x:_) = x


-- Pattern match and recursion
-- Specify that the output can be of any type but needs to be a part of the Num typclass
length' :: (Num b) => [a] -> b -- Get this defintion from ghci :i
length' [] = 0
length' (_:xs) = 1 + length' xs 


-- Pattern match and recursion
sum' :: Num p => [p] -> p
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Guards
-- Note: No "=" after the function definiton
whoYou :: String -> String
whoYou s
    | s == "Sally" = "You are Sally"
    | s == "Hally" = "You are Hally"
    | otherwise = lameName
    where
        lameName = "Lame name"

getPerimeter a b c =  calcPerimeter a b c
    where 
        calcPerimeter :: Int -> Int -> Int -> Int
        calcPerimeter a b c = a + b + c

-- Full function application
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x 

-- Partial function application
compareWithHundred' :: (Num a, Ord a) => a -> Ordering  
compareWithHundred' = compare 100

-- Partial finction application
myMax :: (Num a, Ord a) => a -> a
myMax = max 10


-- Elem implemented with a lambda expression and a foldl
elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys


testWhere :: Integer -> String
testWhere a = 
    case a of
        2 | 2 > 1, let c = 40 -> "Your number is " ++ show c
        a      -> hello a

    where 
        hello::Integer -> String
        hello a = "Your number is not 2, it is " ++ show a


divide :: Integer -> Integer -> Integer
divide a b = divInteger a b