


ladies    = ["Mary","Kitty","Lydia","Elizabeth","Jane"]
gentlemen = ["Charles","Fitzwilliam","George","William"]
couples   = [("Elizabeth","Fitzwilliam"),("Charlotte","William"),("Lydia","George"),("Jane","Charles")]

------------------------- Exercise 1

-- remn=ember lists will have same type
ditch :: Int -> [a] -> [a]
ditch 0 xs = xs
ditch _ [] = []
ditch n xs
   | n >0 = ditch (n-1) (tail xs)


at :: [a] -> Int -> a
at (x:xs) 0 = x
at (x:xs) i 
   | (i > 0 && i<= length(xs)) = at xs (i-1)
   | otherwise = error "Out of bounds"


------------------------- Exercise 2

find :: Eq a => a -> [(a,b)] -> b
find _ [] = error "not found"
find p ((x,y):xs) 
   | p == x  = snd (x,y) -- if p != x , then we get y
   | otherwise = find p xs -- otherwise we keep calling function recursively


which :: Eq a => a -> [a] -> Int
-- here the aux begins with index 0 
which = aux 0 -- this is partially applied 
   where 
  -- here were having index 0 which is int(this is the counter)
  --  and then we have 'a' which is the element specified with that index 
  -- and then we have the give list
      aux :: Eq a => Int -> a -> [a] -> Int
      aux _ x [] = error "which is not found"
      aux i x (y:ys) 
         | x == y = i -- we return the counter 
         | otherwise = aux (i+1) x ys -- if (x != y) , then we increment the counter 'i' and call the function again 


member :: Eq a => [a] -> a -> Bool
member    []  _ = False -- if list is empty then there is no string in that list so return false
member (x:xs) y
    | x == y    = True -- if string y = to the element x , then return true
    | otherwise = member (xs) y -- otherwise call function again but now looking at the xs tail 

remove :: Eq a => [a] -> a -> [a]
remove [] _ = []
remove (x:xs) y 
   | x== y = xs -- we return rest of list
   | otherwise = x: remove xs y -- we call function again but on remaining list

-- changed their type (ord works with inequality)
before :: Ord a => [a] -> [a] -> Bool
before _ [] = False -- if second list is empty then any character wont be before empty
before [] _ = True
before (x:xs) (y:ys)
   | x<y = True
   | x==y = before xs ys 
   | otherwise = False


sorted :: Ord a => [a] -> Bool
sorted  [] = True
sorted [x] =True
sorted (x:y:ys  )
   |x <= y = sorted(y:ys) -- if so then call function again and move onto to next list to check , this can eventually result True
   |otherwise = False

------------------------- Exercise 3

-- we assume each list is sorted 
-- the output is the combination of the sorted list
merge :: Ord a => [a] -> [a] -> [a]

merge xs [] = xs -- if second list is empty then return xs, as thats already sorted
merge [] ys = ys -- if first list is empty then return second list ys, as thats already sorted

-- if both lists are non empty , we look at the first elements 'x' and 'y'
merge (x:xs) (y:ys) 
-- if one element is strictly smaller, that element should become head,
   -- and we call merge of tail of one list and whole of the other

   | x<y = x : merge xs (y:ys) -- x<y , then we merge the tail of xs with remaining y's, and x will be the first new head
   | x== y = x : merge xs ys
   | otherwise = y: merge (x:xs) ys
 
-- nico understandning
merge_2 :: Ord a => [a] -> [a] -> [a]
merge_2 xs [] = xs
merge_2 [] ys = ys
merge_2 (x:xs) (y:ys) 
-- since x<y , then we choose x to be head of list , and then reduce list to xs as we utilised the element
   | x<y = x: merge_2 xs (y:ys)
   -- we are working with both tails now xs and ys, 
   -- as since they were the same element we reduce both lists
   --  as that element is being used as head for new ordered list
   |x==y = y: merge_2 xs ys 

-- we are now working with ys as we used 'y' as the head for new list
   | otherwise = y: merge_2 (x:xs) ys 


-- we are removing elements xs that are also present in ys
minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = ys
minus (x:xs) (y:ys)
   | x == y = minus xs ys -- both same elements we added no elements to list
   | x < y  = x: minus xs (y:ys) -- if x<y then add element to result list
   | otherwise = minus (x:xs) (ys) -- we dont prepend the result as that could come up later in ys


minus_2 :: Ord a => [a] -> [a] ->[a]
minus_2 xs [] =xs
minus_2 [] ys = ys
-- the case where we have non empty lists
minus_2 (x:xs)(y:ys)
   | x<y = x: minus_2 (xs)(y:ys) -- still compating element y with new element in xs
   | x==y = minus_2 (xs)(ys)
   |otherwise = minus_2 (x:xs)(ys)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
-- so we are recursively sorting left and right halves of the list
-- and then we merge 
msort (xs) = merge (msort(take n xs)) (msort(drop n xs)) 
   where 
      n = div (length xs) 2 --dividing length of list by 2 

msort_2 :: Ord a => [a] -> [a]
msort_2 [] = []
msort_2 [x] = [x]
msort_2 xs =  merge (msort_2(take n xs)) (msort_2(drop n xs))
   where 
      n = div (length xs) 2
  