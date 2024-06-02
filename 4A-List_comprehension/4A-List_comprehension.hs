

------------------------- Exercise 1

doubles :: [Int] -> [Int]
-- here we have a list xs
doubles xs = [x *2 | x <- xs ]

odds :: [Int] -> [Int]
odds xs = [x | x <- xs , odd(x)]

doubleodds :: [Int] -> [Int]
-- 2*x is the result operation after gettting the extracted x from list xs
doubleodds xs =[ 2*x | x<- xs, odd(x)] 

shorts :: [String] -> [String]
-- we are removing string that are longer than 5 characters
shorts xs = [x | x<-xs , length(x) <= 5]

-- removed negative numbers and zero, and then squaring the integers
squarePositives :: [Int] -> [Int]
squarePositives xs =[x * x | x <- xs , x>0 ] 

oddLengthSums :: [[Int]] -> [Int]
oddLengthSums xs = [ summing x | x <- xs , odd (length (x))] 
   where 
    -- from the result summing will be receving a lists of integers
    summing :: [Int] -> Int
    summing [] = 0
    summing (x:xs)= x +summing xs
      
-- here we are iterating the list xs and giving the element x , and give the elements where x/=y
remove :: Eq a => [a] -> a -> [a]
remove  xs y = [x | x <- xs , x/=y] 

removeAll :: Eq a => [a] -> [a] -> [a]
-- here we iterated through xs and extracted x, and returned the 'x' which is not in ys
removeAll xs ys  = [x| x <- xs  , not (x `elem` ys) ]

-- here iterated though xs and wrapped it with index i , and selected the index that are odd, and returned that x element
everyother :: [a] -> [a]
-- or you can get : snd (i,x) - > this is the same as just 'x' and returns you the x
everyother xs = [x |(i,x) <- zip [1..] xs , odd i]

same :: Eq a => [a] -> [a] -> [Int]
same xs ys = [i | (i,x,y) <- zip3 [1..] xs ys , x==y ] 


------------------------- Exercise 2

-- here we are producing a combination of items
-- we are iterating through both list p and xs simultaneously

pairs :: [a] -> [b] -> [(a,b)]
pairs p xs= [(z,x) | z <- p , x<- xs] 

selfpairs :: [a] -> [(a,a)]
-- we are returning the values x y , where j>= i for all in xs 
selfpairs xs = [(x,y) | (x,i) <- zip xs [0..] , (y,j) <- zip xs [0..] , j>=  i ]

-- we are using the drop function 
selfpairs_2 :: [a] -> [(a,a)]
-- we are dropping i in xs, and returning what comes after with y
selfpairs_2 xs = [(x,y)| (x,i) <- zip xs [0..] , y <-  drop i xs]

pyts :: Int -> [(Int,Int,Int)]

-- we are getting the elemetns x,y,z from 1 to n , and do the pythagoras , and check x<y, y<z to give up as ordered pythagoras
-- we got oredere pythagorean tripples

pyts n = [(x,y,z) | x<-[1..n] , y <- [1..n] , z<- [1..n] , squared x + squared y == squared z , x<y, y<z]
   where squared x = x *x 

