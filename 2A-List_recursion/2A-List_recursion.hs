

ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]

------------------------- Exercise 1

member :: [String] -> String -> Bool
member    []  _ = False -- if list is empty then there is no string in that list so return false
member (x:xs) y
    | x == y    = True -- if string y = to the element x , then return true
    | otherwise = member (xs) y -- otherwise call function again but now looking at the xs tail 

member' :: [String] -> String -> Bool
member'    []  _ = False  
member' (x:xs) y = x==y || member (xs) y

remove :: [String] -> String -> [String]
remove [] _ = []
remove (x:xs) y 
   | x== y = xs -- we return rest of list
   | otherwise = x: remove xs y -- we call function again but on remaining list



------------------------- Exercise 2

members :: [String] -> [String] -> Bool
members xs    []  = True  -- if list ys is empty then its true as you expect no elements to be in list xs
members xs (y:ys) 
   | y `elem` xs = members xs ys  -- we check if string y in list ys is in list xs, if so then call 'members' but now on remaining elements
   | otherwise = False

members' :: [String] -> [String] -> Bool
members' xs [] = True
members' xs (y:ys) = y `elem` xs && members' xs ys

-- removes every string in ys from the list xs
removeAll :: [String] -> [String] -> [String]
-- need to add the base case when theres no string in list ys, then return whole list xs
removeAll xs [] = xs
removeAll (xs) (y:ys)  = removeAll (remove xs y)  ys 

------------------------- Exercise 3

before :: [Char] -> [Char] -> Bool
before _ [] = False -- if second list is empty then any character wont be before empty
before [] _ = True
before (x:xs) (y:ys)
   | x<y = True
   | x==y = before xs ys 
   | otherwise = False

before' :: [Char] -> [Char] -> Bool
before' _ [] = False
before' [] _ = True
before' (x:xs) (y:ys) = (x<y) || (x==y && before' xs ys)  -- check if x<y or x==y and we recursilvely go onto next string

sorted :: [String] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:ys)
   | x <= y = sorted (y:ys) -- check if x<y , and then proceed to check rest of list
   | otherwise = False


