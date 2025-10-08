# Haskell Programming - DSA Contributions

Welcome to the Haskell folder for DSA (Data Structures and Algorithms) contributions!

## Structure

Organize your code by topic:
- `arrays/` - Array and list-related problems
- `linked_lists/` - Custom linked list implementations and problems
- `stacks/` - Stack implementations and problems
- `queues/` - Queue implementations and problems
- `trees/` - Tree data structures and algorithms
- `graphs/` - Graph algorithms
- `sorting/` - Sorting algorithms
- `searching/` - Searching algorithms
- `dynamic_programming/` - DP problems
- `greedy/` - Greedy algorithms
- `backtracking/` - Backtracking problems
- `math/` - Mathematical algorithms
- `strings/` - String manipulation problems

## Naming Convention

- Use camelCase for filenames: `binarySearch.hs`, `bubbleSort.hs`
- Include problem name in filename if solving a specific problem
- Use descriptive module names that match the filename

## Code Guidelines

- Include type signatures for all top-level functions
- Add comments explaining the algorithm approach
- Include time and space complexity in comments
- Use meaningful variable names
- Leverage Haskell's functional programming paradigms
- Include example usage in comments

## Compilation and Running

To compile and run Haskell files:
```bash
ghc filename.hs
./filename
```

Or use the interpreter:
```bash
ghci filename.hs
```

## Example Structure

```haskell
-- | Binary search implementation
-- Time Complexity: O(log n)
-- Space Complexity: O(log n) due to recursion
module BinarySearch where

binarySearch :: (Ord a) => [a] -> a -> Maybe Int
binarySearch xs target = binarySearchHelper xs target 0 (length xs - 1)
  where
    binarySearchHelper [] _ _ _ = Nothing
    binarySearchHelper xs target low high
      | low > high = Nothing
      | xs !! mid == target = Just mid
      | xs !! mid > target = binarySearchHelper xs target low (mid - 1)
      | otherwise = binarySearchHelper xs target (mid + 1) high
      where
        mid = (low + high) `div` 2
```