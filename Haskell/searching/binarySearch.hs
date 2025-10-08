-- | Binary search implementation in Haskell
-- Time Complexity: O(log n)
-- Space Complexity: O(log n) due to recursion
module BinarySearch where

-- | Binary search function that returns the index of target element
-- Returns Nothing if element is not found
binarySearch :: (Ord a) => [a] -> a -> Maybe Int
binarySearch [] _ = Nothing
binarySearch xs target = binarySearchHelper xs target 0 (length xs - 1)

-- | Helper function for binary search with bounds
binarySearchHelper :: (Ord a) => [a] -> a -> Int -> Int -> Maybe Int
binarySearchHelper xs target low high
  | low > high = Nothing
  | xs !! mid == target = Just mid
  | xs !! mid > target = binarySearchHelper xs target low (mid - 1)
  | otherwise = binarySearchHelper xs target (mid + 1) high
  where
    mid = (low + high) `div` 2

-- | Iterative binary search implementation
-- Time Complexity: O(log n)
-- Space Complexity: O(1)
binarySearchIterative :: (Ord a) => [a] -> a -> Maybe Int
binarySearchIterative xs target = search 0 (length xs - 1)
  where
    search low high
      | low > high = Nothing
      | xs !! mid == target = Just mid
      | xs !! mid > target = search low (mid - 1)
      | otherwise = search (mid + 1) high
      where
        mid = (low + high) `div` 2

-- Example usage:
-- >>> binarySearch [1,2,3,4,5,6,7,8,9,10] 5
-- Just 4
-- >>> binarySearch [1,2,3,4,5,6,7,8,9,10] 11
-- Nothing

main :: IO ()
main = do
  let sortedList = [1,2,3,4,5,6,7,8,9,10]
  putStrLn "Binary Search Examples:"
  putStrLn $ "Searching for 5 in " ++ show sortedList ++ ": " ++ show (binarySearch sortedList 5)
  putStrLn $ "Searching for 11 in " ++ show sortedList ++ ": " ++ show (binarySearch sortedList 11)
  putStrLn $ "Iterative search for 7: " ++ show (binarySearchIterative sortedList 7)