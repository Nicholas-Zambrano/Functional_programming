
------------------------- Exercise 1

doubles :: [Int] -> [Int]
doubles (xs) = map (double) xs
   where
    double   :: Int -> Int
    double x = x * 2

odds :: [Int] -> [Int]
-- we are filtering through the odd number 
odds xs = filter (odd)  xs

doubleodds :: [Int] -> [Int]
doubleodds xs =  map double (filter odd xs)
   where 
    double :: Int ->Int 
    double x = x * 2 


------------------------- Exercise 2

-- the filter function is a predicate which is a bool type 
shorts :: [String] -> [String]
shorts xs = filter long xs
   where 
    -- returns bool type
    long :: String -> Bool
    long x 
       | length (x) <= 5 = True
       |otherwise = False

squarePositives :: [Int] -> [Int]
-- we are mapping the squares function onto a list which is filtered to contain positive integers
squarePositives xs = map squares (filter positive xs)
   where 
    squares :: Int -> Int
    squares x = x * x

    positive :: Int -> Bool
    positive x 
       | x>0 = True
       | otherwise = False

oddLengthSums :: [[Int]] -> [Int]

oddLengthSums xs = map sums (filter oddLengths xs)
  where 
    oddLengths :: [Int] -> Bool
    oddLengths x 
       | odd (length x) = True
       | otherwise = False
    
    sums :: [Int] -> Int
    sums [] = 0
    sums (x:xs) = x + sums xs



------------------------- Exercise 3

remove :: Eq a => [a] -> a -> [a]
remove [] _ = [] -- if no list then theres nothing to remove 
remove (xs) y = filter (notEqual) xs
   where 
    -- notEqual :: a -> Bool
    -- dont need to specify y as the 'notEqual' is in scope and has access to parameters i.e 'y'
    notEqual  x
       | x /= y = True
       | otherwise = False

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll [] ys = []
removeAll xs [] = xs
-- removing every string in ys from xs
removeAll xs (ys) = filter (notIn) xs
   where 
    notIn x
       |  not (x `elem` ys) = True
       | otherwise = False

-- indexing elements in a list by paring them with the index
numbered :: [a] -> [(Int,a)]
numbered xs = zip [1..] xs

-- getting every other element from list
everyother :: Eq a => [a] -> [a]
everyother xs = map second (filter oddIndex (numbered_2 xs))
   where 
    -- here we are counting the elements with index
    numbered_2 :: Eq a => [a] -> [(Int,a)]
    numbered_2 xs = zip[1..] xs

    -- here we are saying if the index is odd then its true, that will be our predicate   
    oddIndex x
       | odd (fst x) = True
       | otherwise = False
    -- we are selecing the second element in the pair
    second x = snd x

-- two lists and returns a list of the positions where their elements coincide
same :: Eq a => [a] -> [a] -> [Int]
same xs ys = map fst (filter characters_selected (zip [1..] (zipWith (==) xs ys)))
   where 
    characters_selected  x = snd x
    