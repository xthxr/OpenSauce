-- | Quick sort implementation in Haskell
-- Time Complexity: Average O(n log n), Worst O(n²)
-- Space Complexity: O(log n) average, O(n) worst case
module QuickSort where

-- | Quick sort implementation using list comprehensions
-- This is a pure functional approach typical in Haskell
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
  let smallerSorted = quickSort [a | a <- xs, a <= x]
      biggerSorted = quickSort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

-- | Alternative quicksort using filter
quickSortFilter :: (Ord a) => [a] -> [a]
quickSortFilter [] = []
quickSortFilter (pivot:rest) = 
  quickSortFilter smaller ++ [pivot] ++ quickSortFilter larger
  where
    smaller = filter (<= pivot) rest
    larger = filter (> pivot) rest

-- | In-place style quicksort (more efficient for large lists)
quickSortInPlace :: (Ord a) => [a] -> [a]
quickSortInPlace [] = []
quickSortInPlace [x] = [x]
quickSortInPlace xs = 
  let pivot = head xs
      rest = tail xs
      (smaller, larger) = partition (<= pivot) rest
  in quickSortInPlace smaller ++ [pivot] ++ quickSortInPlace larger
  where
    partition :: (a -> Bool) -> [a] -> ([a], [a])
    partition _ [] = ([], [])
    partition p (x:xs)
      | p x = (x:left, right)
      | otherwise = (left, x:right)
      where (left, right) = partition p xs

-- Example usage:
-- >>> quickSort [3,6,8,10,1,2,1]
-- [1,1,2,3,6,8,10]
-- >>> quickSort "hello"
-- "ehllo"

main :: IO ()
main = do
  let unsortedList = [64, 34, 25, 12, 22, 11, 90]
  putStrLn "Quick Sort Examples:"
  putStrLn $ "Original: " ++ show unsortedList
  putStrLn $ "Sorted: " ++ show (quickSort unsortedList)
  
  let stringToSort = "quicksort"
  putStrLn $ "\nSorting string '" ++ stringToSort ++ "': " ++ quickSort stringToSort
  
  putStrLn $ "\nUsing filter version: " ++ show (quickSortFilter unsortedList)
  putStrLn $ "Using in-place version: " ++ show (quickSortInPlace unsortedList)