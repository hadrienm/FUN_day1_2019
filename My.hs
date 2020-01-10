mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x
    | x < 0 = True
    | otherwise = False

myAbs :: Int -> Int
myAbs x
    | x < 0 = x * (-1)
    | otherwise = x

myMin :: Int -> Int -> Int
myMin x y
    | x < y = x
    | y < x = y
    | otherwise = x

myMax :: Int -> Int -> Int
myMax x y
    | x < y = y
    | y < x = x
    | otherwise = x

myTuple :: a -> b -> (a, b)
myTuple a b = (a , b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b , c)

myFst :: (a, b) -> a
myFst(a,b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

mySwap :: (a, b) -> (b, a)
mySwap(a,b) = (b,a)

myHead :: [a] -> a
myHead [] = error "Empty"
myHead(x:y) = x

myTail :: [a] -> [a]
myTail (x:y) = y

myLength :: [a] -> Int
myLength (_:y) = 1 + myLength (y)
myLength [] = 0

myNth :: [a] -> Int -> a
myNth [] x = error "Empty"
myNth (x:y) b
    | b /= 0 = myNth y (b - 1)
    | otherwise = x

myTake :: Int -> [a] -> [a]
myTake x [] = error "Empty"
myTake a b
    | a > myLength(b) = b
myTake a (x:y)
    | a /= 1 = (x:(myTake (a - 1) y))
    | a == 1 = [x]

myDrop :: Int -> [a] -> [a]
myDrop x [] = error "Empty"
myDrop x y
    | x >= myLength(y) = []
myDrop b (x:y)
    | b /= 1 = myDrop (b - 1) y
    | otherwise = y

myAppend :: [a] -> [a] -> [a]
myAppend [] y = y
myAppend (x : xs) y = x : myAppend xs y

myReverse :: [a] -> [a]
myReverse (x:xs) = myAppend (myReverse xs) [x]
myReverse [] = []

myInit :: [a] -> [a]
myInit [] = error "Empty"
myInit a = myTake (myLength(a) - 1) a

myLast :: [a] -> a
myLast [] = error "Empty"
myLast a = myHead (myReverse a)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a:as) (b:bs) = (a, b) : myZip as bs

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((a, b):xs) = (a:as , b:bs)
    where (as, bs) = myUnzip xs

myMap :: (a -> b) -> [a] -> [b]
myMap a [] = []
myMap a (x:xs) = a x : map a xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter a [] = []
myFilter a (x:xs)
    | a x == True = x : myFilter a xs
    | otherwise = myFilter a xs

myNotFilter :: (a -> Bool) -> [a] -> [a]
myNotFilter a [] = []
myNotFilter a (x:xs)
    | a x == False = x : myNotFilter a xs
    | otherwise = myNotFilter a xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl a b [] = b
myFoldl a b (x:xs) = myFoldl a (a b x) xs 

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr a b [] = b
myFoldr a b (x:xs) = a x (myFoldr a b xs)

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan a [] = ([], [])
mySpan a b = (myFilter a b, myNotFilter a b)