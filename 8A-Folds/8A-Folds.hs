
import Data.Char

------------------------- a

allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs)= (x==True) && allTrue xs 

allTrue_2 :: [Bool] -> Bool
allTrue_2 xs = all (== True) xs

allTrue_3 :: [Bool] -> Bool 
allTrue_3 = foldr (&&) True 


------------------------- b

longestLength :: [[a]] -> Int
longestLength []= error "no length"
-- when we have one element then we just get the length of the element
longestLength [x] =  length x
longestLength (x:y:xs) 
-- if length x>y than we keep of hold x and call the funtion xs 
    | length (x) > length (y) = longestLength(x:xs)
    -- if theyre the same then keep hold of any element and then compare the next
    | length x == length y = longestLength (x:xs)
    -- if y>x, then keep hold of y and call the next elements
    | otherwise = longestLength (y:xs)

longestLength_1_1 :: [[a]]->Int
longestLength_1_1 [x] = length x
longestLength_1_1 (x:xs) = max (length x) (longestLength_1_1 xs )

longestLength_2 :: [[a]] -> Int
longestLength_2 xs = maximum [length x | x<- xs ]

longestLength_3 :: [[a]] -> Int
longestLength_3 xs = foldr1 (max) (map length xs)

------------------------- c

sumOddSquares :: [Int] -> Int
-- so given a list of  integers
-- i created two functions
-- this functions 'squaring_odd_numbers' takes the list of numbers and returns list of numbers 
-- and within that function it its odd then square it and prepend it 
-- otherwise keep calling the functions

-- after that i created a sum function , that takes a list of integers and return returns Int
-- this sums the list which will contain squared odd numbers
sumOddSquares (xs) = sum (squaring_odd_numbers xs)

   where 
    squaring_odd_numbers :: [Int] -> [Int]
    squaring_odd_numbers [] = []
    squaring_odd_numbers (x:xs)
       | odd x =  (x*x : squaring_odd_numbers xs)
       | otherwise = squaring_odd_numbers xs

    sum :: [Int] ->Int
    sum [] = 0
    sum (x:xs) = x + sum xs

-- summing over the list of squared odds
sumOddSquares_2 :: [Int] -> Int
sumOddSquares_2 xs  = sum [x*x | x<- xs , odd x]

sumOddSquares_3 :: [Int] -> Int
-- the base case is 0
-- the final is that we are summing 
sumOddSquares_3 xs = foldr (+) 0 (map (squaring) (filter odd xs))
   where squaring x=x*x

-- composition operator
sumOddSquares_4 :: [Int] -> Int
sumOddSquares_4  = foldr (+) 0 . (map (squaring) . (filter odd ))
   where squaring x = x*x


------------------------- d

shortFWords :: [String] -> Bool

-- here we used !! index operator to access the first element 
shortFWords [] = False
shortFWords (x:xs) 
    | length(x) == 4 && (x !! 0 == 'F') = True
    | otherwise =  shortFWords xs

-- here we used in built head function to get the first character
shortFWords_1 :: [String] -> Bool
shortFWords_1 [] =False
shortFWords_1 (x:xs) 
   | length x == 4 && head x == 'F' = True
   | otherwise = shortFWords_1 xs

-- for boleans in list comprehenstion you need to add function outside the list comprehension
-- as the list comprehenstion just gives you modified list 
shortFWords_2 :: [String] -> [String]
-- without the boolean it will just produce a list containg true or false 
shortFWords_2 xs =  [ x  | x<- xs , length x == 4, x !! 0 == 'F' ]

shortFWords_21 :: [String] -> Bool
shortFWords_21 xs = or [x !! 0 == 'F' | x<-xs, length x ==4]

shortFWords_3 :: [String] -> Bool
-- and the result (||) we apply this to the list , as we are working with a list of booleans 
-- we assumed that list is non empty
shortFWords_3 xs = foldr1 (||) (map short xs)
    where 
      -- we apply map to give a boolean values in the list
      -- map is dealgin with individual string , hence why type is like that
      short :: String -> Bool
      short (x)
         | (length x == 4) && head x =='F' = True
         |otherwise = False

-- this is the base case - if its empty then we return false
shortFWords_31 :: [String] -> Bool
shortFWords_31 xs = foldr (||) (False) (map short xs)
   where 
      short :: String -> Bool
      short x 
         | length x == 4 &&  x!!0 =='F' = True
         |otherwise = False 





shortFWords_32 :: [String] -> Bool
shortFWords_32 xs = foldr (||) (False) (map check_character (filter length_4 xs))
   where 
      length_4 :: String -> Bool
      length_4 (x) 
         | length x == 4 = True
         | otherwise = False
      check_character :: String -> Bool
      check_character x
         | x !! 0 == 'F' = True
         |otherwise = False
         
-- composition 
shortFWords_33 :: [String] -> Bool
shortFWords_33 = foldr (||) False . map ((== 'F') . head) . filter ((== 4) .length)



------------------------- e

-- first im goint to zip the characters
zipping :: [(Char,Int)]
zipping = [(x,i) | (x,i) <- zip ['a'.. 'z'] [1..]]


-- adding words 
addWords :: String -> [(Char,Int)] -> [Int]
addWords [] _ = []
addWords (x:xs) (ys) 
   |  x' `elem`  map fst ys  =  snd (matching_value ys x'): addWords xs ys
   | otherwise = addWords xs ys 
   
   where 
      x' = toLower x
      matching_value :: [(Char,Int)] -> Char -> (Char,Int)

      matching_value ys x' = head [(c,score)| (c,score)<- ys , c==x']

wordScore :: String -> Int
wordScore x = sum (addWords x zipping)



-- returning score of its letters 

-- here we are zipping characters with int
zipping_2 :: [(Char,Int)]
zipping_2 = [(x,i) |(x,i) <- zip ['a' .. 'z'] [1..] ]

-- here we are taking a string, and list of character that are paired with integers,
-- the goal is to get a list of integers for that string, which will be summed later on 
addWords_2 :: String -> [(Char,Int)]->[Int]
addWords_2 [] _ = []
addWords_2 (x:xs) ys 
-- need to convert x to lower case 
-- we are checking if x lower case is an element of the first character in the tuble, e.g 'a' `elem` map fist (`a`, 1)
   -- then we apeend the corresponding value 

   -- we are checking if the lower case x , is present in a list of chacaters in which we just mapped
   -- matching_value takes a list of tuples and a character x , and returns (Char,Int),
   --  and we take second element of matching_value , which is an Integer
   -- and then after we recurse to the next character in xs m and ys remains a constant paramter
   | x' `elem` map fst ys =  snd (matching_value_2  ys x') : addWords_2 xs ys
   -- if that element is not in lisy ys, then we recurse xs  the string
   | otherwise = addWords_2 xs ys
   
   where 
      --  we are putting x into lower case
      x' = toLower x 
-- takes a list of tuples , and a 'character' in which we process in the addWords_2 function 
      matching_value_2 :: [(Char,Int)] -> Char -> (Char,Int)
      -- we use the head to return the tuple , and not the list and tuple
      -- and we go over each tuple in the list of ys , and return the tupe where c== x' (this is character)
      matching_value_2 ys x' = head [(c,score) | (c,score) <- ys , c == x']

wordScore_2 :: String ->Int
wordScore_2 [] = 0
-- we arer calling the string x and zipping_2 which is the list of character with its integer
wordScore_2 x = sum (addWords_2 x zipping_2)

wordScore_21 :: String -> Int

wordScore_21 (x:xs) 
   | 1 <= i && i <= 26 = i + wordScore xs
   | otherwise = wordScore xs

   where 
      -- we are subtracting 64 from the unicode pointof the character
      i = subtract 64 (ord(toUpper x))


wordScore_3 :: String -> Int
-- here we create a list of integers from the string, and then we apply our funcion unicode_integer x
--  we check if our transformed x and check and return the ones within the range 1 and 26 as thats the length of the alpahbet
wordScore_3 xs = sum [ unicode_integer x| x <- xs  , 1 <= unicode_integer x , unicode_integer x<=26 ]
   where 
      unicode_integer x = subtract 64 (ord(toUpper x))

wordScore_4 :: String -> Int
-- the base case is 0 as no string then the result is simply 0 
wordScore_4 xs = foldr (+) 0 (filter  check_length ( map unicode_integer xs) )
   where 
      unicode_integer :: Char -> Int
      unicode_integer x = subtract 64 (ord (toUpper x))

-- the check length , takes an integer, as mapping will giving us integer values
-- remember that filter checks individual elements
      check_length :: Int -> Bool
      check_length x 
         | (1 <= x && x<=26)  = True
         | otherwise = False
        





-- wordScore :: String -> Int
-- wordScore = undefined
------------------------- f

concatCheapWords :: [String] -> String
concatCheapWords [] = []
concatCheapWords (x:xs) 
   | wordScore_4  x <=42 = ' ' : x ++ concatCheapWords xs 
   | otherwise  = concatCheapWords xs





concatCheapWords_2 :: [String] -> String
concatCheapWords_2 [] = []
concatCheapWords_2 (x:xs)
   | wordScore_21 x <=42  =  ' '  : x  ++ concatCheapWords_2 xs
   | otherwise = concatCheapWords_2 xs


concatCheapWords_3 :: [String] -> String
concatCheapWords_3 xs = concating [ x | x <- xs , wordScore x <= 42]
   where 
      --  remember when concatting at the end, you are taking a list of strings
      -- as list comprehensions produces lists 
      concating :: [String] -> String
      concating [] = []
      concating (x:xs) = ' ' : x ++ concating xs
 

concatCheapWords_31 :: [String] -> String
concatCheapWords_31 xs = concating_2 [x | x<-xs, wordScore x <= 42]
   where 
      -- concating is taking a list of strings produced by list comprehension 
      concating_2 :: [String] -> String
      -- always take care of the base case:
      concating_2 [] = []
      -- adding spaces and concatenating with rest of string
      concating_2 (x:xs) = ' ' : x ++ concating_2 xs 


concatCheapWords_41 :: [String]-> String
concatCheapWords_41 xs = foldl (++) [] (map (' ':) (filter (check_length) xs))
    where 
      check_length :: String -> Bool
      check_length x 
         | wordScore x <= 42 = True
         | otherwise = False

      

concatCheapWords_42 :: [String] -> String
concatCheapWords_42 = foldl (++) [] . map(' ' :) . (filter ((<=42) . wordScore)) 


concatCheapWords_4 :: [String]-> String
concatCheapWords_4 xs = foldl (++)[] (map adding_space  (filter checking_length xs))
   where 
      checking_length :: String -> Bool
      checking_length (x)
         | wordScore x <= 42 = True
         | otherwise = False
      
      adding_space :: String -> String
      adding_space x = ' ': x




