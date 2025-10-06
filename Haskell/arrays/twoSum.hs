-- | Two Sum problem implementation in Haskell
-- Given an array of integers and a target sum, return indices of two numbers that add up to target
-- Time Complexity: O(n²) for brute force, O(n) for hash map approach
-- Space Complexity: O(1) for brute force, O(n) for hash map approach
module TwoSum where

import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)

-- | Brute force approach - check all pairs
-- Time Complexity: O(n²), Space Complexity: O(1)
twoSumBruteForce :: (Eq a, Num a) => [a] -> a -> Maybe (Int, Int)
twoSumBruteForce xs target = findPair (zip xs [0..]) target
  where
    findPair [] _ = Nothing
    findPair ((val, idx):rest) target =
      case findTarget val idx rest target of
        Just secondIdx -> Just (idx, secondIdx)
        Nothing -> findPair rest target
    
    findTarget val1 idx1 pairs target =
      case [(val2, idx2) | (val2, idx2) <- pairs, val1 + val2 == target] of
        [] -> Nothing
        ((_, idx2):_) -> Just idx2

-- | Hash map approach using Data.Map
-- Time Complexity: O(n), Space Complexity: O(n)
twoSumHashMap :: (Ord a, Num a) => [a] -> a -> Maybe (Int, Int)
twoSumHashMap xs target = twoSumHelper xs target Map.empty 0
  where
    twoSumHelper [] _ _ _ = Nothing
    twoSumHelper (x:rest) target seen idx =
      let complement = target - x
      in case Map.lookup complement seen of
        Just complementIdx -> Just (complementIdx, idx)
        Nothing -> twoSumHelper rest target (Map.insert x idx seen) (idx + 1)

-- | Find all pairs that sum to target
-- Time Complexity: O(n²), Space Complexity: O(n)
twoSumAllPairs :: (Eq a, Num a) => [a] -> a -> [(Int, Int)]
twoSumAllPairs xs target = 
  [(i, j) | (val1, i) <- indexed, (val2, j) <- indexed, 
   i < j, val1 + val2 == target]
  where
    indexed = zip xs [0..]

-- | Two sum with sorted array (two pointers approach)
-- Time Complexity: O(n), Space Complexity: O(1)
-- Assumes input array is sorted
twoSumSorted :: (Ord a, Num a) => [a] -> a -> Maybe (Int, Int)
twoSumSorted xs target = twoPointers 0 (length xs - 1) xs target
  where
    twoPointers left right xs target
      | left >= right = Nothing
      | sum' == target = Just (left, right)
      | sum' < target = twoPointers (left + 1) right xs target
      | otherwise = twoPointers left (right - 1) xs target
      where
        sum' = xs !! left + xs !! right

-- Example usage:
-- >>> twoSumBruteForce [2,7,11,15] 9
-- Just (0,1)
-- >>> twoSumHashMap [3,2,4] 6
-- Just (1,2)

main :: IO ()
main = do
  let nums1 = [2, 7, 11, 15]
  let target1 = 9
  putStrLn "Two Sum Examples:"
  putStrLn $ "Array: " ++ show nums1 ++ ", Target: " ++ show target1
  putStrLn $ "Brute Force: " ++ show (twoSumBruteForce nums1 target1)
  putStrLn $ "Hash Map: " ++ show (twoSumHashMap nums1 target1)
  
  let nums2 = [3, 2, 4]
  let target2 = 6
  putStrLn $ "\nArray: " ++ show nums2 ++ ", Target: " ++ show target2
  putStrLn $ "Brute Force: " ++ show (twoSumBruteForce nums2 target2)
  putStrLn $ "Hash Map: " ++ show (twoSumHashMap nums2 target2)
  
  putStrLn $ "All pairs for target " ++ show target2 ++ ": " ++ show (twoSumAllPairs nums2 target2)
  
  let sortedNums = [1, 2, 3, 4, 6]
  putStrLn $ "\nSorted array: " ++ show sortedNums ++ ", Target: 6"
  putStrLn $ "Two pointers: " ++ show (twoSumSorted sortedNums 6)