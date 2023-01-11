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

identityProgression :: Integral x => x -> [x]
identityProgression j = createProgression (identity) 1 j

factorial ::(Integral x) => x -> x
factorial n = foldl (*) 1 $ identityProgression n

binomialCoefficient :: (Integral x, Fractional y) => x -> x -> y
binomialCoefficient m k =
  (fromIntegral mFactorial) / (fromIntegral denominator)
  where mFactorial = factorial m
        kFactorial = factorial k
        denominator = kFactorial * (factorial (m - k))

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
  let factorial = identityProgression n
  putStrLn "5! is"
  print $ foldl (*) 1 factorial

--Evaluate binomial coefficients
problem_1_1_3 :: IO ()
problem_1_1_3 = do
  let m = 7
  let k = 4
  putStrLn "Binomial coefficient where m = 7 and k = 4"
  print $ binomialCoefficient m k
