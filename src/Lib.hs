module Lib (diaDoCuringAbbr, diaDoCuringa, isLeapYear, isLeapYearInt, countDays, fixDay, fixYear, feb, seasons, validDate) where

-- types

data Suit = Ouros | Paus | Copas | Espadas | Joker deriving (Read, Show, Enum, Eq, Ord)
data CardValue = As | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | Valete | Dama | Rei | Curinga | CuringaDuplo deriving (Read,  Show, Enum, Eq, Ord)
data Card = Card {value :: CardValue, suit :: Suit} deriving (Read, Eq) 
instance Show Card where
    show (Card value suit) = show value ++ " de " ++ show suit ++ " "

type Day = Int
type Month = Int
type Year = Int
type Leap = Int
type Date = (Day, Month, Year)

-- frode

isLeapYear :: Year -> Bool
isLeapYear y = divisibleBy 400 || (divisibleBy 4 && not (divisibleBy 100))
    where divisibleBy x = rem y x == 0

isLeapYearInt :: Year -> Int
isLeapYearInt year = if isLeapYear year 
                     then 1
                     else 0 

fixDay :: Year -> Day -> Int
fixDay year day = if day > limit year
                  then day - 60
                  else day + 305
    where limit year = 60 - (isLeapYearInt year)

fixYear :: Year -> Int
fixYear year | year < 1790 = 1790 - year
             | otherwise = year - 1790

suitYear :: Year -> Int
suitYear year = (fixYear(year) `quot` 13) `rem` 4

cardYear :: Year -> Int
cardYear year = fixYear year `rem` 13

seasons :: Day -> Year -> Int
seasons day year = seasonsByDay day $ isLeapYearInt year

seasonsByDay :: Day -> Leap -> Int
seasonsByDay day leap | day <= (62 - leap) = 1
                    | day <= (154 - leap) = 2
                    | day <= (247 - leap) = 3
                    | day <= (338 - leap) = 0
                    | day <= (367 - leap) = 1
                    | otherwise = 1

cardMonth :: Int -> Int
cardMonth day = (day `quot` 28) `rem` 13

suitWeek :: Int -> Int
suitWeek day = ((day `quot` 7)  `quot` 13) `rem` 4

cardWeek :: Int -> Int
cardWeek day = (day `quot` 7) `rem` 13

suitDay :: Int -> Int
suitDay day | day == 0 = 4
            | otherwise = ((day - 1) `quot` 13) `rem` 4

cardDay :: Int -> Int
cardDay day | day == 0 = 13 
            | otherwise = (day - 1) `rem` 13

feb :: Day -> Year -> Bool            
feb day year = day <= (28 + isLeapYearInt(year))

validMonthWithThirtyDays :: Month -> Bool
validMonthWithThirtyDays month = month == 4 || month == 6 || month == 9 || month == 11

validDay :: Day -> Month -> Year -> Bool
validDay day month year | day < 1 = False 
                   | month == 2 = feb day year
                   | day == 30 = validMonthWithThirtyDays month
                   | day < 32 = True
                   | otherwise = False

validMonth :: Month -> Bool
validMonth month = month > 0 &&  month < 13 

validYear :: Year -> Bool
validYear year = year > 0

validDate :: Date -> Bool
validDate (day, month, year) = validDay day month year && validMonth month && validYear year

dayOfYear :: Date -> Int
dayOfYear date = if validDate date
    then countDays date
    else 0

dayMonth :: Day -> Month -> Leap -> Int
dayMonth day month leap | month == 1 = day
                        | month == 2 = day + 31
                        | month == 3 = day + 59 + leap
                        | month == 4 = day + 90 + leap
                        | month == 5 = day + 120 + leap
                        | month == 6 = day + 151 + leap
                        | month == 7 = day + 181 + leap
                        | month == 8 = day + 212 + leap
                        | month == 9 = day + 243 + leap
                        | month == 10 = day + 273 + leap
                        | month == 11 = day + 304 + leap
                        | month == 12 = day + 334 + leap
                        | otherwise = 0

countDays :: Date -> Int
countDays (day, month, year) = dayMonth day month $ isLeapYearInt year

dayCard :: Int -> Card
dayCard nDays = Card { 
    value = (toEnum $ cardDay nDays), suit = (toEnum $ suitDay nDays) 
    }

weekCard :: Int -> Card
weekCard nDays = Card { 
    value = (toEnum $ cardWeek nDays), suit = (toEnum $ suitWeek nDays) 
    }

monthCard :: Int -> Day -> Month -> Card
monthCard nDays day month = Card { 
    value = (toEnum $ cardMonth nDays), suit = (toEnum $ seasons day month) 
    }

yearCard :: Year -> Card
yearCard year = Card { 
    value = (toEnum $ cardYear year), suit = (toEnum $ suitYear year) 
    }

-- For better view in console

dayShow :: Int -> String
dayShow days = "Dia " ++ (show $ dayCard days) ++ "\n" 

weekShow :: Int -> String
weekShow days = "Semana " ++ (show $ weekCard days) ++ "\n" 

monthShow :: Int -> Day -> Month -> String
monthShow days day month = "Mes " ++ (show $ monthCard days day month) ++ "\n" 

yearShow :: Year -> String
yearShow year = "Ano " ++ (show $ yearCard year) ++ "\n"

cardString :: Date -> String
cardString date@(day, month, year) = "\n" ++ dayShow n ++ weekShow n ++ monthShow n day month ++ yearShow year
    where n = fixDay year $ dayOfYear date
                  
diaDoCuringa :: String -> String
diaDoCuringa input = cardString $ parseDate input

-- For better view in tests
data Suit' = O_ | P_ | C_ | E_ | J_ deriving (Read, Show, Enum, Eq, Ord)
data CardValue' = I1 | I2 | I3 | I4 | I5 | I6 | I7 | I8 | I9 | I10 | IJ | IQ | IK | IJo | IJd deriving (Read,  Show, Enum, Eq, Ord)

data Card' = Card' {v :: CardValue', s :: Suit'} deriving (Read, Eq) 
instance Show Card' where
        show (Card' v s) = show v ++ show s

dayCard' :: Int -> Card'
dayCard' nDays = Card' { 
    v = (toEnum $ cardDay nDays), s = (toEnum $ suitDay nDays) 
    }

weekCard' :: Int -> Card'
weekCard' nDays = Card' { 
    v = (toEnum $ cardWeek nDays), s = (toEnum $ suitWeek nDays) 
    }

yearCard' :: Int -> Card'
yearCard' nDays = Card' { 
    v = (toEnum $ cardYear nDays), s = (toEnum $ suitYear nDays) 
    }

monthCard' :: Int -> Day -> Month -> Card'
monthCard' nDays day month = Card' { 
    v = (toEnum $ cardMonth nDays), s = (toEnum $ seasons day month) 
    }

diaDoCuringAbbr :: Date -> String
diaDoCuringAbbr date@(day, month, year) = (show $ dayCard' n) ++ (show $ weekCard' n) ++ (show $ monthCard' n day month) ++ (show $ yearCard' year)
    where n = (fixDay year $ dayOfYear date)

-- input
parseDate :: String -> Date
parseDate input = (l!!0, l!!1, l!!2) -- HIRO fix this to a funtional way
    where l = map (read::String->Int) $ words input