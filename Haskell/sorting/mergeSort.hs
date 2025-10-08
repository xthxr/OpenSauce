-- | Merge sort implementation in Haskell
-- Time Complexity: O(n log n)
-- Space Complexity: O(n)
module MergeSort where

-- | Merge sort implementation
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
  where
    (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs

-- | Merge two sorted lists into one sorted list
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- | Alternative merge sort with explicit splitting
mergeSortExplicit :: (Ord a) => [a] -> [a]
mergeSortExplicit xs
  | length xs <= 1 = xs
  | otherwise = merge (mergeSortExplicit left) (mergeSortExplicit right)
  where
    mid = length xs `div` 2
    left = take mid xs
    right = drop mid xs

-- | Bottom-up merge sort implementation
mergeSortBottomUp :: (Ord a) => [a] -> [a]
mergeSortBottomUp [] = []
mergeSortBottomUp xs = mergePairs (map (:[]) xs)
  where
    mergePairs [ys] = ys
    mergePairs yss = mergePairs (pairs yss)
    
    pairs [] = []
    pairs [ys] = [ys]
    pairs (ys1:ys2:yss) = merge ys1 ys2 : pairs yss

-- Example usage:
-- >>> mergeSort [38,27,43,3,9,82,10]
-- [3,9,10,27,38,43,82]

main :: IO ()
main = do
  let unsortedList = [38, 27, 43, 3, 9, 82, 10]
  putStrLn "Merge Sort Examples:"
  putStrLn $ "Original: " ++ show unsortedList
  putStrLn $ "Sorted (standard): " ++ show (mergeSort unsortedList)
  putStrLn $ "Sorted (explicit): " ++ show (mergeSortExplicit unsortedList)
  putStrLn $ "Sorted (bottom-up): " ++ show (mergeSortBottomUp unsortedList)
  
  let characters = ['m','e','r','g','e','s','o','r','t']
  putStrLn $ "\nSorting characters: " ++ show characters
  putStrLn $ "Result: " ++ mergeSort characters