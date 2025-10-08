-- | Fibonacci sequence implementations in Haskell
-- Demonstrates different approaches: naive recursion, memoization, and iterative
-- Time and space complexity analysis for each approach
module Fibonacci where

-- | Naive recursive Fibonacci (inefficient)
-- Time Complexity: O(2^n), Space Complexity: O(n)
fibonacciNaive :: Integer -> Integer
fibonacciNaive 0 = 0
fibonacciNaive 1 = 1
fibonacciNaive n = fibonacciNaive (n-1) + fibonacciNaive (n-2)

-- | Fibonacci using infinite list (lazy evaluation)
-- Time Complexity: O(n), Space Complexity: O(n)
fibonacciList :: [Integer]
fibonacciList = 0 : 1 : zipWith (+) fibonacciList (tail fibonacciList)

-- | Get nth Fibonacci number from the infinite list
-- Time Complexity: O(n), Space Complexity: O(n)
fibonacciNth :: Int -> Integer
fibonacciNth n = fibonacciList !! n

-- | Iterative Fibonacci using accumulator
-- Time Complexity: O(n), Space Complexity: O(1)
fibonacciIterative :: Integer -> Integer
fibonacciIterative n = fibHelper n 0 1
  where
    fibHelper 0 a _ = a
    fibHelper n a b = fibHelper (n-1) b (a+b)

-- | Fibonacci using memoization with list
-- Time Complexity: O(n), Space Complexity: O(n)
fibonacciMemo :: Int -> Integer
fibonacciMemo n = fibs !! n
  where
    fibs = map fibMemo [0..]
    fibMemo 0 = 0
    fibMemo 1 = 1
    fibMemo k = fibs !! (k-1) + fibs !! (k-2)

-- | Matrix exponentiation approach for Fibonacci
-- Time Complexity: O(log n), Space Complexity: O(log n)
fibonacciMatrix :: Integer -> Integer
fibonacciMatrix 0 = 0
fibonacciMatrix n = let [[a, _], [_, _]] = matrixPower [[1, 1], [1, 0]] (n-1)
                    in a

-- | Matrix multiplication for 2x2 matrices
matrixMultiply :: [[Integer]] -> [[Integer]] -> [[Integer]]
matrixMultiply [[a, b], [c, d]] [[e, f], [g, h]] = 
  [[a*e + b*g, a*f + b*h], [c*e + d*g, c*f + d*h]]

-- | Matrix exponentiation
matrixPower :: [[Integer]] -> Integer -> [[Integer]]
matrixPower _ 0 = [[1, 0], [0, 1]]  -- Identity matrix
matrixPower m 1 = m
matrixPower m n
  | even n = matrixPower (matrixMultiply m m) (n `div` 2)
  | otherwise = matrixMultiply m (matrixPower m (n-1))

-- | Golden ratio approach (Binet's formula)
-- Time Complexity: O(1), Space Complexity: O(1)
-- Note: Limited by floating point precision for large n
fibonacciBinet :: Integer -> Integer
fibonacciBinet n = round $ (phi^n - psi^n) / sqrt5
  where
    phi = (1 + sqrt 5) / 2  -- Golden ratio
    psi = (1 - sqrt 5) / 2  -- Conjugate of golden ratio
    sqrt5 = sqrt 5

-- | Generate first n Fibonacci numbers
-- Time Complexity: O(n), Space Complexity: O(n)
fibonacciSequence :: Int -> [Integer]
fibonacciSequence n = take n fibonacciList

-- | Check if a number is a Fibonacci number
-- Time Complexity: O(log n), Space Complexity: O(1)
isFibonacci :: Integer -> Bool
isFibonacci n = isPerfectSquare (5*n*n + 4) || isPerfectSquare (5*n*n - 4)
  where
    isPerfectSquare x = let root = floor (sqrt (fromInteger x))
                        in toInteger (root * root) == x

-- | Sum of first n Fibonacci numbers
-- Time Complexity: O(n), Space Complexity: O(1)
fibonacciSum :: Int -> Integer
fibonacciSum n = fibonacciNth (n + 2) - 1

-- Example usage and benchmarking
main :: IO ()
main = do
  putStrLn "Fibonacci Sequence Implementations:"
  
  let n = 20
  putStrLn $ "\nFirst " ++ show n ++ " Fibonacci numbers:"
  putStrLn $ show (fibonacciSequence n)
  
  putStrLn $ "\nFibonacci(" ++ show n ++ ") using different methods:"
  putStrLn $ "Naive recursion: " ++ show (fibonacciNaive (fromIntegral n))
  putStrLn $ "Infinite list: " ++ show (fibonacciNth n)
  putStrLn $ "Iterative: " ++ show (fibonacciIterative (fromIntegral n))
  putStrLn $ "Memoized: " ++ show (fibonacciMemo n)
  putStrLn $ "Matrix exponentiation: " ++ show (fibonacciMatrix (fromIntegral n))
  putStrLn $ "Binet's formula: " ++ show (fibonacciBinet (fromIntegral n))
  
  putStrLn $ "\nSum of first " ++ show n ++ " Fibonacci numbers: " ++ show (fibonacciSum n)
  
  putStrLn "\nChecking if numbers are Fibonacci:"
  let testNumbers = [0, 1, 2, 3, 5, 8, 13, 21, 34, 35, 55]
  mapM_ (\x -> putStrLn $ show x ++ " is Fibonacci: " ++ show (isFibonacci x)) testNumbers
  
  putStrLn "\nPerformance note:"
  putStrLn "- Naive recursion: Very slow for large n (exponential time)"
  putStrLn "- Infinite list: Good balance of elegance and performance"
  putStrLn "- Iterative: Most memory efficient"
  putStrLn "- Matrix exponentiation: Best for very large n (logarithmic time)"