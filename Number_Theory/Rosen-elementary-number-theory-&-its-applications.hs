{-# LANGUAGE ScopedTypeVariables #-}

module NumberTheory
( summation
) where
import System.IO



--1.1 helper functions
summation :: Num a => [a] -> a
summation [] = 0
summation (x:xs) = x + summation xs

createProgression :: (Num x, Integral y) => (y -> x) -> y -> y -> [x]
createProgression f j n
  | n < j = []
  | n >= j = [(f j)] ++ (createProgression f (j+1) n)

geometric :: (Floating x, Integral y) => x -> x -> y -> x
geometric a r j = a * (r ** (fromIntegral j))

identity :: (Num x, Integral y) => (y -> x)
identity t = fromIntegral t

range :: Integral x => x -> [x]
range j = createProgression (identity) 1 j

rangeI :: Integral x => x -> x -> [x]
rangeI i j = createProgression (identity) i j

factorial ::(Integral x) => x -> x
factorial n = foldl (*) 1 $ range n

binomialCoefficient :: (Integral x, Fractional y) => x -> x -> y
binomialCoefficient m k =
  (fromIntegral mFactorial) / (fromIntegral denominator)
  where mFactorial = factorial m
        kFactorial = factorial k
        denominator = kFactorial * (factorial (m - k))
  
--for problem 1.1.4
data ListAndSize x = ListAndSize [x] Int deriving (Show, Read)
getList :: ListAndSize x -> [x]
getList (ListAndSize xs y) = xs

getSize :: ListAndSize x -> Int
getSize (ListAndSize xs y) = y

getListAndSize :: [x] -> ListAndSize x
getListAndSize xs = ListAndSize xs (length xs)


--for problem 1.1.5
getMissing :: (Integral x) => x -> x -> x
getMissing x y
  | x + y == 1 = 2
  | x + y == 3 = 0
  | x + y == 2 = 1

--let the result be a list of pairs
--each pair represents the index of the ring you're moving from and the index of the ring you're moving to
--assumes there are 3 posts, all rings of the given number and less are stacked on currentStack
towerOfHanoiSolver :: (Integral x) => x -> x -> x -> [[x]]
towerOfHanoiSolver numRings currentStack goalPost
  | numRings == 1 = [[currentStack, goalPost]] -- if we have one ring just move to our goal
  | otherwise = towerOfHanoiSolver (numRings - 1) currentStack (getMissing goalPost currentStack) --solve the substack to the opposite post
                ++ [[currentStack, goalPost]] -- move over to our goal post
                ++ towerOfHanoiSolver (numRings - 1) (getMissing goalPost currentStack) goalPost -- the substack is now on the opposite post, solve it back to our post

--- PROBLEMS 1.1
--Find the sum of the terms of a geometric series 
problem_1_1_1 :: IO ()
problem_1_1_1 = do
  let ourGeometricProgression = createProgression (geometric 1 2) 1 5
  print ourGeometricProgression
  print "the summation is" 
  print $ foldl (+) 0 ourGeometricProgression

--Evaluate n!
problem_1_1_2 :: IO ()
problem_1_1_2 = do
  let n = 5
  let factorial = range n
  putStrLn "5! is"
  print $ foldl (*) 1 factorial

--Evaluate binomial coefficients
problem_1_1_3 :: IO ()
problem_1_1_3 = do
  let m = 7
  let k = 4
  putStrLn "Binomial coefficient where m = 7 and k = 4"
  print $ binomialCoefficient m k


--Print out pascals triangle, (i'll do first 10 rows)
problem_1_1_4 :: IO ()
problem_1_1_4 = do
  let numRows = rangeI 0 10
  let triangle = map range numRows
  let listSizeTriangle = map getListAndSize triangle
  let missingFirstNumber = map mapInner listSizeTriangle
  let pascalsTriangle = map ([1.0] ++) missingFirstNumber
  mapM_ print pascalsTriangle
  where
    mapInner (ListAndSize xs y) = map (binomialCoefficient y) xs

--List the moves in tower of hanoi
problem_1_1_5 :: IO ()
problem_1_1_5 = do
  --tower of hanoi inputs:
  --4 rings
  --moving from post 0 to post 2
   let solution =  towerOfHanoiSolver 4 0 2
   print solution
  --tower of hanoi output:
  
  --a list of pairs [x,y]. The player needs to take the top ring from the post at index x and move it to the post at index y
  --there are 3 posts indexed 0, 1, and 2

  --example output [[0,1],[0,2],[1,2]]
  --this would mean we take the top ring from the left most post (index 0) and move it to the middle post (index 1)
  --then we move the next ring from the left most post and move it all the way to the right
  --finally we take the middle ring, the first one we moved, and put it on the far right post. This is the solution for a 2 ring tower of hanoi (towerOfHanoiSolver 2 0 2)
 

--Expand (x+y)^n, where n is a positive integer using the binomial theorem
--The idea is to create 3 lists, one representing the [m k] binomial coefficient value
-- one representing the x part and another list representing the y part
-- at the end we can just multiply all 3 together
problem_1_1_6 :: IO ()
problem_1_1_6 = do
  let n = 5 :: Int
  let x = 3.0 :: Float
  let y = 7.0 :: Float
  --we'll have to create a list of binomial coefficients from [n, 0] to [n, n]
  --start with a list from 0 to n
  let nums = rangeI 0 n
  --now we can make each number a pair [x,y]. The numbers we have fit y so x is always just n
  let binomialInts = map (\a -> [n, a]) nums :: [[Int]]
  let binomials = map (\a -> [fromIntegral n, fromIntegral a]) nums :: [[Float]]
  print "our binomial portion"
  print binomials
  
  let xValues = map (binomialToXs x) binomials
  print "x portion"
  print xValues
  
  let yValues = map (binomialToYs y) binomials
  print "y portion"
  print yValues
  
  let binomialsSolved = map (\(a:b:_) -> binomialCoefficient (toInteger a)  (toInteger b)) binomialInts
  print "binomail portion"
  
  print binomialsSolved
  print "all together now"
  let xsAndYs = zipWith (*) xValues yValues
  let allTogether = zipWith (*) binomialsSolved xsAndYs
  print allTogether
  
  print "summed"
  print $ foldl (+) 0 allTogether
  
  where
    binomialToXs :: (Floating a) => a -> [a] -> a
    binomialToXs x (a:b:_) = x ** (a - b)
    binomialToYs :: (Floating a) => a -> [a] -> a
    binomialToYs y (a:b:_) =  y ** b
