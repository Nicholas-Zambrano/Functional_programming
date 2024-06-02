
-- import System.Random

------------------------- Exercise 1

repeatMe :: IO ()
repeatMe = do

   x <-getLine  -- getting the user input
   putStr "You just told me " -- just printing this 
   putStrLn x -- and repeating the user input 





-- no input is being given everything is coming out from the console
repeatMe_2 :: IO ()
repeatMe_2 = do 
  putStrLn "whats your name"
  x <- getLine 
  putStr "You just told me huh, my name is huh: "
  putStrLn ("yes mate your name is " ++ x)


------------------------- Exercise 2

lizzy :: IO ()
lizzy = do
  putStrLn "Dr. Lizzy -- Good morning, how are you today. Please tell me what’s on your mind."

  lizzyLoop

lizzyLoop :: IO ()
lizzyLoop = do
  str <- getLine -- we are getting what the user inputted

  if str == "Exit"
    then do 
      putStrLn exit
  else do 

  -- r <- randomIO :: IO Int
  -- putStrLn (randomresponse str r) -- its responding to what the user said

    putStrLn (response str) -- printint user input alongside the response
    lizzyLoop








lizzy_2 :: IO()
lizzy_2 = do

  putStrLn "Dr. Lizzy -- Good morning, how are you today. Please tell me what’s on your mind."
  lizzyLoop_2 -- we combined the other function , this created a loop 


lizzyLoop_2 :: IO()
lizzyLoop_2 = do

  -- the user responese is a string
  x <- getLine

  if x ==  "Exit"
    then do 
      putStrLn exit
  else do
     putStrLn (response x) -- here we are responding to the user
     lizzyLoop_2 



-- countdown function , takes an intgeger and performs IO actions

countdown :: Int ->IO()
countdown n = do 
  putStrLn (show n )
  if n == 0 
    then do 
      putStrLn "Liftoff !!!"
    else do 
      countdown (n-1)


------------------------- Exercise 3

-------------------------

welcome :: String
welcome =
  "\nDr. Lizzy -- Good morning, how are you today. Please tell me what's on your mind.\n"

exit :: String
exit =
  "\nDr. Lizzy -- Thank you for coming in today. I think we made good progress. I will see you next week at the same time.\n"

response :: String -> String
response str =
  "\nDr. Lizzy -- " ++ x ++ y ++ "\n"
     where
       x = responses1 !! (length str `mod` 3)
       y = responses2 !! (length str `mod` 5)
       responses1 =
         ["Interesting that you say \"" ++ str ++ "\"\n"
         ,"Hmm... "
         ,"Let\'s examine that more closely, shall we.\n"
         ]
       responses2 =
         ["Please tell me more about that."
         ,"How does that make you feel?"
         ,"Now, why do you mention that?"
         ,"Do you think this has something to do with your mother?"
         ,"Go on."
         ]

randomresponse :: String -> Int -> String
randomresponse str r =
  "\nDr. Lizzy -- " ++ x ++ y ++ "\n"
     where
       x = responses1 !! (r `mod` 3)
       y = responses2 !! (r `mod` 5)
       responses1 =
         ["Interesting that you say \"" ++ str ++ "\"\n"
         ,"Hmm... "
         ,"Let\'s examine that more closely, shall we.\n"
         ]
       responses2 =
         ["Please tell me more about that."
         ,"How does that make you feel?"
         ,"Now, why do you mention that?"
         ,"Do you think this has something to do with your mother?"
         ,"Go on."
         ]




------------------------- Lambda-calculus

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

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


-------------------------

numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (numeral' i))
  where
    numeral' i
      | i <= 0    = Variable "x"
      | otherwise = Apply (Variable "f") (numeral' (i-1))


-------------------------

variables :: [Var]
variables = [ [x] | x <- ['a'..'z'] ] ++ [ x : show i | i <- [1..] , x <- ['a'..'z'] ]

removeAll :: [Var] -> [Var] -> [Var]
removeAll xs ys = [ x | x <- xs , not (elem x ys) ]

fresh :: [Var] -> Var
fresh = head . removeAll variables


rename :: Var -> Var -> Term -> Term
rename x y (Variable z)
    | z == x    = Variable y
    | otherwise = Variable z
rename x y (Lambda z n)
    | z == x    = Lambda z n
    | otherwise = Lambda z (rename x y n)
rename x y (Apply n m) = Apply (rename x y n) (rename x y m)


substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y)
    | x == y    = n
    | otherwise = Variable y
substitute x n (Lambda y m)
    | x == y    = Lambda y m
    | otherwise = Lambda z (substitute x n (rename y z m))
    where z = fresh (used n `merge` used m `merge` [x,y])
substitute x n (Apply m p) = Apply (substitute x n m) (substitute x n p)


------------------------- Exercise 4


beta :: Term -> [Term]
-- this is a lambda abstraction applied to beta 
beta (Apply (Lambda x n) m) = 
  -- here we substituting the argument m into the body n, in every occurence in x (this is the bound variable)
  [substitute x m n] ++
  -- this is a beta reduced version n 
-- exploring the beta reductions within the body 
  [Apply (Lambda x n') m | n' <- beta n] ++
  -- exploring all beta reductions within the argument m 
  [Apply (Lambda x n ) m' | m' <- beta m ] 

-- now looking at the application case 
beta (Apply n m )= 
  -- exploring the possible beta reduction in function n 
  [Apply n' m | n' <- beta n  ] ++
  -- exploring the beta reducition in function m 
  [Apply n m' | m' <- beta m ]

-- exploring the beta reduction in the body 'n' of the lambda abstraction 
beta (Lambda x n ) = [Lambda  x n' | n' <- beta n ]

-- there is no beta reducing a variable 
beta (Variable _ ) = []



beta_2 :: Term -> [Term]
-- when you have lambda abstraction and an argument
beta_2 (Apply (Lambda x n) m) = 

-- here we are substituting n into the body m
  [substitute x  m n ] ++

  [Apply (Lambda x n') m | n' <- beta n ]  ++
  [Apply (Lambda x n) m' | m' <- beta m]

-- the next case is just application of 2 termns

beta_2 (Apply n m ) = 
  [Apply n' m | n' <- beta n ] ++ 
  [Apply n m' | m' <- beta  m]

-- next case is simply a lambda abstraction with no argument
-- we are reducing the body n 
beta_2 (Lambda x n ) = 
  [Lambda x n' |n' <- beta n  ]  

-- last case when we have a variable we do nothing, it does not beta reduce
beta_2 (Variable x) = []



normalize :: Term -> IO ()
-- this normalize function takes n as input 
normalize n = do
  print n -- printing current term to the console
-- this calculates all possible reduction of the current term 
  let ns = beta n -- we are storing the possible reduction, we call the beta funciton to find the possible reductions 
  -- this means if the current term is in normal form then we return 
  if null ns then
    return ()
    -- if not 
  else do
    -- ns is list of possible reductions
    -- normalise on the next term , we do this iteratively until term reaches normal form 
    normalize (head ns)






-- is taking a term and then performing input and output actions
normalize_2 :: Term -> IO ()
normalize_2 n = do 
  print n 

-- list of reducions 
  let ns = beta n
  -- if no more reductions then stop
  if null ns
    then  do 
      return ()
  else do
    -- take the first term and continue normalizing
    normalize_2 (head ns)
