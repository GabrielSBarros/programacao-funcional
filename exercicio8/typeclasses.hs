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
