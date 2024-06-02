

------------------------- Exercise 1

square :: Int -> Int
square x = x * x

-- taking 3 integer arguments and returns a boolean
-- or you could write it in one line
-- pythagoras a b c = square a +square b == square c
pythagoras :: Int -> Int -> Int -> Bool
pythagoras a b c  
   |( square a + square b == square c) = True
   | otherwise = False






------------------------- Exercise 2

factorial :: Int -> Int
factorial x
    | x <= 1    = 1
    | otherwise = x * factorial (x-1)

euclid :: Int -> Int -> Int
euclid x y
    | x == y = x
    -- taking the euclid of the smaller one and difference between x and y
    | x <  y = euclid x (y-x)
    | x >  y = euclid (x-y) y

power :: Int -> Int -> Int
power x y 
   | y ==0 = 1
   | y> 0 = x * power x (y-1)
   | otherwise = error "power: negative exponent "
--note: you will need to create your own cases,
--      replacing the equals (=) sign with guards



------------------------- Exercise 3

range :: Int -> Int -> [Int]
-- range 0 0 = []
-- range _ _ = []
-- range _ _ =[]
range x y 
   | x > y = error "that is not a range"
   | x==y = [x]
   | otherwise = x : range (x+1) y
--note: you will need to create your own guards
--      and add your own parameters

times :: [Int] -> Int
times [] = 1 -- times anyting by 0 then anser is 1 , e.g 1^0 =1
times (x:xs) = x * times xs
--note: you will need to create your own pattern-matching

fact :: Int -> Int
-- fact 0 = 1
-- it has to be from range 1 , as smallest factorial is 1
fact  x = times (range 1 x)