
data Tree a = Empty | Node a (Tree a) (Tree a)

t :: Tree Int 
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))

instance Show a => Show (Tree a) where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _     = False

------------------------- Exercise 1

member :: Ord a => a -> Tree a  -> Bool
member _ Empty = False
member x (Node y left right ) 
   | x==y = True
  --  here we are traversing through left branch
   | x< y = member x left
  --  x>y , then we traverse through right branch whilst checking x 
   |otherwise = member x right

largest :: Tree a -> a
largest Empty            = error "empty tree"
largest (Node x left Empty) = x -- if node has left child and no right child then the current node is the greater than
largest (Node x left right)     = (largest right) -- since right branch has larger values we traverse trhough there

smallest :: Tree a -> a
smallest Empty = error "cant find min"
smallest (Node x Empty right ) = x
smallest (Node x left right ) =  (smallest left )-- recursing trhough right tree, as that contains larger values


-- flattening the tree to produce list of integers
flatten ::  Tree a -> [a]
flatten Empty = []
flatten (Node x left right) = flatten left ++ [x] ++ flatten right

-- we chanhe
ordered :: Ord a=> Tree a -> Bool
-- ordered [] = True
ordered Empty = True
ordered (Node x left right) = check (flatten (Node x left right))
-- ordered (Node x left right) =  check (flatten left ++ [x] ++ flatten right)
   where 
    -- check ::Ord a => [a] -> Bool
    check [] = True
    check [x] = True
    check (x:y:xs) = x <= y && (check (y:xs))


flatten_2 :: Tree a -> [a]
flatten_2 Empty = []
flatten_2 (Node x left right  ) = (flatten_2 left) ++ [x] ++ (flatten_2 right)

-- in our oredered_2 , we made the type Ord a => as it includes checking , e.g in our where function 
ordered_2 :: Ord a => Tree a -> Bool
ordered_2 Empty = False
-- ordered_2 (Node x left right)  = isSorted (flatten_2 left ++ [x] ++ flatten_2 right)
ordered_2 tree = isSorted (flatten_2 tree)
    where 
      isSorted :: Ord a=> [a] -> Bool -- dont need to specify
      isSorted [] =True
      isSorted [x] = True
      isSorted (x:y:xs) = x<= y && (isSorted (y:xs))

-- find largest element then 
deleteLargest :: Tree a -> Tree a
deleteLargest  Empty = error "There is no largest"
deleteLargest (Node x left Empty) = Empty -- when there is no right branch then largest node is there and we replace with 'Empty'
-- here we construct the tree , but we traverse on the right branch, which eventually will be replace by Empty
deleteLargest (Node x left right) = Node x left (deleteLargest right ) 

-- THIS HAS ordering , so we use 'Ord', this provides functions like <= etc
delete :: Ord a=> a -> Tree a -> Tree a
delete _ Empty = Empty -- no tree , then return no tree
delete y (Node x left right)
-- y<x, then recursing on the left branch whilst checking y == x
    | y < x     = Node  x (delete y left) right
    -- if y>x then recursing on right branch
    | y > x     = Node x left (delete y right)
    --if y==x then it checks if left subtree is empty then return right subtree, this wont ruin the ordering 
    | isEmpty left = right

    -- if left subtree is not empty
    -- we are replacing the deleted element with a value immediately smaller and is the largest on the left subtree
    | otherwise = Node (largest left) (deleteLargest left) right







------------------------- Exercise 2


-- instance Show a => Show (Tree a) where
--     show = unlines . aux ' ' ' '
--       where
--         aux _ _ Empty = []
--         aux c d (Node x s t) = 
--           [ c:' ':m | m <- aux ' ' '|' s ] ++ 
--           ['+':'-':show x] ++ 
--           [ d:' ':n | n <- aux '|' ' ' t ]




------------------------- Lambda-calculus

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 1
  where
      -- Index k means:
      --   1 : top level - no parentheses
      --   2 : function position - parentheses around abstractions
      --   3 : argument position - parentheses around abstractions and applications
      f _ (Variable x)   = x
      f k (Lambda x m)   = if elem k [2,3] then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 1 m 
      f k (Apply  m n)   = if elem k   [3] then "(" ++ s ++ ")" else s where s = f 2 m ++ " " ++ f 3 n

instance Show Term where
  show = pretty

n1 :: Term
n1 = Lambda "x" (Variable "x")

n2 :: Term
n2 = Lambda "x" (Apply (Lambda "y" (Variable "x")) (Variable "z"))

n3 :: Term
n3 = Apply (Lambda "x" (Lambda "y" (Apply (Variable "x") (Variable "y")))) n1

-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys)
    | x <  y    = x : minus    xs (y:ys)
    | x == y    =     minus    xs    ys
    | otherwise =     minus (x:xs)   ys

-------------------------

used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda x n) = [x] `merge` used n
used (Apply  n m) = used n `merge` used m

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x n) = free n `minus` [x]
free (Apply  n m) = free n `merge` free m


------------------------- Exercise 3

-- function numberal takes integer i , and returns a Term
-- this function generates church numeral representation of integer 'i' 
numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (numeral' i))
   where 
    numeral' i
       | i<= 0 = Variable "x"
      --  if i > 0 then recurvsively construct church numeral 
       | otherwise = Apply (Variable "f") (numeral' (i-1)) 



------------------------- Exercise 4

variables :: [Var]  
variables = [[x] | x <- ['a' .. 'z']] ++ [x: show i | i <- [ 1..] , x <- ['a' .. 'z'] ]

variables_2 :: [Var]
variables_2 = [[x] | x <- ['a'.. 'z']] ++ [x:show i | i <- [1..] , x <- ['a'.. 'z']] 


-- we are removing all string in ys
removeAll ::  [Var] -> [Var] -> [Var]
removeAll xs [] = xs
removeAll [] ys = []
removeAll (xs) (ys)  = [ x | x <- xs , not (x `elem` ys)]

fresh :: [Var] -> Var
-- we are removing variables present in the input list
fresh (xs) =  get_first (removeAll (variables) (xs)) -- this will return a list of alphabets
   where 
    -- here we just select the first variable
    get_first :: [Var] -> Var -- the type signature for the helper function should return same type as the original function 
    get_first (x:xs) = x 

rename :: Var -> Var -> Term -> Term

-- here we are reanaming x to y 
rename x y (Variable z)
    | z == x    = Variable y
    | otherwise = Variable z

rename x y (Lambda z n)
    | z == x    = (Lambda z n )
    | otherwise = Lambda z ((rename x y) n)

rename x y (Apply n m) = Apply ( ((rename x y) n))  (((rename x y) m) )


-- nico doing it 
rename_2 :: Var -> Var -> Term -> Term

rename_2 x y (Variable z) 
   | z == x = Variable y
   | otherwise = Variable z
rename_2 x y (Lambda z n)
   | z == x = Lambda z n
   | otherwise = (Lambda z (rename_2 x y n))
  --  we do renaming for both terms , as we have cases such as lambda and variable which handles it
rename_2 x y (Apply n m) =  Apply (rename_2 x y n) (rename_2 x y m)



substitute :: Var -> Term -> Term -> Term
substitute x n  (Variable y) 
   | y == x = n
   | otherwise = Variable y

substitute x n (Lambda y m) 
   | y == x = (Lambda y m )
   | otherwise =  (Lambda z (substitute x n (rename y z m)))
   where z = fresh (used n `merge` used m `merge` [x,y])

substitute x y ( Apply m n  ) =  Apply ( (substitute x y ) m) ((substitute x y ) n)



substitution_2 :: Var -> Term -> Term -> Term
substitution_2 x n (Variable y)
   | y == x = n -- this is a term 
   | otherwise = Variable y

substitution_2 x n (Lambda y m )
    | y== x = Lambda y m
    |otherwise= Lambda z (substitution_2 x n (rename_2 y z m) )
    where 
      z = fresh (used n `merge` used m `merge` [x,y])
    
substitution_2 x y (Apply m n  ) = Apply ((substitution_2 x y )m) ((substitution_2 x y )n)


