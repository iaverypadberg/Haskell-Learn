-- Circle and Rectangle are the value constructors
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show

-- Enum stuff
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)  

-- Type synonym
type AlsoDay = Day

-- String is implemented as a type synonym
type String = [Char]