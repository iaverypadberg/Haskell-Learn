import Data.Char

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