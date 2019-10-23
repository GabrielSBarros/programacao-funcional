-- elem :: (Eq a, Foldable t) => a -> t a -> Bool
-- sum :: (Num a, Foldable t) => t a -> a
-- minimum :: (Ord a, Foldable t) => t a -> a
{- 


-}

data Part = AM | PM deriving(Eq, Show)
data Time = Local Int Int Part | Total Int Int

time1 = Local 4 30 PM
time2 = Total 16 30
time3 = Local 4 30 AM
time4 = Total 4 30

totalMinutos (Total a b) = (a * 60) + b
totalMinutos (Local a b AM) = (a * 60) + b
totalMinutos (Local a b PM) = (a + 12) * 60 + b

instance Eq Time where
    a == b = totalMinutos a == totalMinutos b    


instance Show Time where
    show (Local a b c) = show a ++ ":" ++ show b ++ " " ++ show c
    show (Total a b) = show a ++ ":" ++ show b

instance Ord Time where
    x < y = (totalMinutos x) < (totalMinutos y)
    x <= y = (totalMinutos x) <= (totalMinutos y)
    x >= y = (totalMinutos x) >= (totalMinutos y)
    x > y = (totalMinutos x) > (totalMinutos y)
    min x y = if x <= y then x else y
    max x y = if x >= y then x else y

seleciona time timeList = filter (>= time) timeList

toTotal :: Int -> Time
toTotal a = (Total (div a 60) (mod a 60))

-- instance Enum Time where    
--     succ (Total a b) = Total a (b + 1)
--     toEnum a = (totalMinutos a)
--     fromEnum a = toTotal a