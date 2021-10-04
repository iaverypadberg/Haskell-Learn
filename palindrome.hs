
import Data.Char
main :: IO()
main = 
    do 
        word <- getLine 
        print(verbosePal word)

talk :: String -> IO()
talk = putStrLn 

equalToA:: String -> IO()
equalToA a = 
    case(a == "aha") of
        True -> putStrLn "A is equal to a"
        False -> putStrLn "Not equal to a :/"


isPalindrome :: String -> Bool
isPalindrome word =
    case( word == reverse word) of
        True -> True
        False -> False


-- Make this a maybe bool because we can return nothing for a false input
isEmpty :: String -> Maybe Bool
isEmpty word = 
    case word of
        "" -> Nothing 
        _ -> Just (callPalindromeLower word) -- In every case except the empty string!


verbosePal :: String -> String
verbosePal word = 
    case isEmpty word of
        Just True -> "A Palindrome!"
        Just False -> "Not a Palindrome!"
        Nothing -> "No word entered!"

allLowerCase :: String -> String
allLowerCase word = myMap toLower word


callPalindromeLower :: String -> Bool
callPalindromeLower word = 
    isPalindrome (removeSpaces (allLowerCase word))

-- Remove all white spaces in a string
removeSpaces :: String -> String
removeSpaces list = 
    case list of
        [] -> []
        ' ': remainder -> removeSpaces remainder -- Case where the first item in the string is a space
        something : remainder -> something : removeSpaces remainder -- Case where there is something other than a blank space at the head of the string

-- -- Implement our own map function over a list of things
myMap :: (a -> a) -> [a] -> [a]
myMap func list = 
    case list of
        [] -> []
        (firstElem : rest) ->  func firstElem : myMap func rest

-- Implementation of head function
myHead :: [a] -> a
myHead (beg : end) = beg




