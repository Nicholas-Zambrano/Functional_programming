
data IntTree = Empty | Node Int IntTree IntTree
  -- deriving Show

t :: IntTree
t = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 5 Empty (Node 6 Empty Empty))


------------------------- Exercise 1

isEmpty :: IntTree -> Bool
isEmpty Empty = True
isEmpty _     = False

rootValue :: IntTree -> Int
rootValue Empty        =  0 
rootValue (Node i _ _) = i

height :: IntTree -> Int

height Empty = 0 
-- the node is one higher than its subtree
-- we are getting the max height, if its in the left subtree or right
height (Node x left right)= 1 + max (height left) (height right)

member :: Int -> IntTree -> Bool
member _ Empty = False
member x (Node y left right) 
-- when that integer we are checking = that y value , we return true
   | x==y = True
  --  if x<y then we recursive on the left subtree 
   | x<y =  (member x left)
  --  x>y we recursive on the right subtree
   | x>y = (member x right)

paths :: Int -> IntTree -> [[Int]]
paths x Empty =[]
paths x (Node y left right)
   | x == y = [[]] ++ map (0 :) (paths x left) ++ map (1 :) (paths x right)
   | otherwise = map (0 : ) (paths x left ) ++ map (1:) (paths x right) 


-------------------------


-- we provided our manual show instance, and for this to work we commented out the deriving 'show function' 
instance Show IntTree where
    show = unlines . aux ' ' ' '
      where
        aux _ _ Empty = []
        aux c d (Node x s t) = 
          [ c:' ':m | m <- aux ' ' '|' s ] ++ 
          ['+':'-':show x] ++ 
          [ d:' ':n | n <- aux '|' ' ' t ]



------------------------- Exercise 2

type Var = String  
  
-- the tupe has 3 constructors
data Term = 
  Variable Var 
  -- the term is the body of the of the lambda abstraction
  |  Lambda Var Term
  | Apply Term Term

--  deriving Show



example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))



-- this just displays the lambda terms nicely
pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

-- defining a custom show instance for the Term data type
instance Show Term where
  show = pretty

n1 :: Term
n1 = Lambda  "x" (Variable "x")

n2 :: Term
n2 = Lambda "x" (Apply (Lambda "y" (Variable "x") ) (Variable "z"))

-- apply has 2 terms, if not then you could get the error 'too few arguments'
n3 :: Term
n3 = Apply (Lambda "x" (Lambda "y" (Apply (Variable "x")(Variable "y"))) ) (Lambda "x" (Variable "x"))

-- collecting the used variables in a term 
used :: Term -> [Var]
-- if the case is a variable 

used  (Variable x)  =  [x] -- constructs list containing variable x 
--  'x' is the variable being abstracted , and 'n' is the body
used (Lambda x n )= [x] `merge` used  n -- x = ['x'] , n = ['x','y'] , ['x'] `merge` ['x','y'] = ['x','y']

used (Apply n m) =  used n `merge` used m -- collecting the used variables

-- we are using the data type 'term' and poducing [string]
used_2 :: Term -> [Var]
used_2 (Variable x ) = [x] -- the used variable x is just ['x']
used_2 (Lambda x n) =  [x] `merge` used n
used_2 (Apply n m) = used n `merge` used m 



free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x n) =  free n `minus` [x] 
free (Apply n m) = free n `merge` free m


free_2 :: Term -> [Var]
free_2 (Variable x) = [x] -- just 'x' itself is a free variable
free_2 (Lambda x n ) = free_2 n `minus` [x] -- with lambda expression we minus it with binded variable to give us the free variable
free_2 (Apply n m )  = free_2 n `merge` free_2 m  -- here we merged it normally as we handle the case of lambda expression 


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
