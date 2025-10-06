-- | Binary Tree implementation in Haskell
-- Includes tree construction, traversals, and common operations
module BinaryTree where

-- | Binary Tree data type
data BinaryTree a = Empty 
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Show, Eq)

-- | Insert a value into a binary search tree
-- Time Complexity: O(log n) average, O(n) worst case
-- Space Complexity: O(log n) average, O(n) worst case
insertBST :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insertBST val Empty = Node val Empty Empty
insertBST val (Node nodeVal left right)
  | val <= nodeVal = Node nodeVal (insertBST val left) right
  | otherwise = Node nodeVal left (insertBST val right)

-- | Search for a value in binary search tree
-- Time Complexity: O(log n) average, O(n) worst case
-- Space Complexity: O(log n) average, O(n) worst case
searchBST :: (Ord a) => a -> BinaryTree a -> Bool
searchBST _ Empty = False
searchBST val (Node nodeVal left right)
  | val == nodeVal = True
  | val < nodeVal = searchBST val left
  | otherwise = searchBST val right

-- | In-order traversal (Left -> Root -> Right)
-- Time Complexity: O(n), Space Complexity: O(n)
inOrderTraversal :: BinaryTree a -> [a]
inOrderTraversal Empty = []
inOrderTraversal (Node val left right) = 
  inOrderTraversal left ++ [val] ++ inOrderTraversal right

-- | Pre-order traversal (Root -> Left -> Right)
-- Time Complexity: O(n), Space Complexity: O(n)
preOrderTraversal :: BinaryTree a -> [a]
preOrderTraversal Empty = []
preOrderTraversal (Node val left right) = 
  [val] ++ preOrderTraversal left ++ preOrderTraversal right

-- | Post-order traversal (Left -> Right -> Root)
-- Time Complexity: O(n), Space Complexity: O(n)
postOrderTraversal :: BinaryTree a -> [a]
postOrderTraversal Empty = []
postOrderTraversal (Node val left right) = 
  postOrderTraversal left ++ postOrderTraversal right ++ [val]

-- | Calculate the height of the tree
-- Time Complexity: O(n), Space Complexity: O(log n) average
treeHeight :: BinaryTree a -> Int
treeHeight Empty = 0
treeHeight (Node _ left right) = 
  1 + max (treeHeight left) (treeHeight right)

-- | Count total number of nodes
-- Time Complexity: O(n), Space Complexity: O(log n) average
nodeCount :: BinaryTree a -> Int
nodeCount Empty = 0
nodeCount (Node _ left right) = 1 + nodeCount left + nodeCount right

-- | Find minimum value in BST
-- Time Complexity: O(log n) average, O(n) worst case
findMin :: BinaryTree a -> Maybe a
findMin Empty = Nothing
findMin (Node val Empty _) = Just val
findMin (Node _ left _) = findMin left

-- | Find maximum value in BST
-- Time Complexity: O(log n) average, O(n) worst case
findMax :: BinaryTree a -> Maybe a
findMax Empty = Nothing
findMax (Node val _ Empty) = Just val
findMax (Node _ _ right) = findMax right

-- | Check if tree is a valid BST
-- Time Complexity: O(n), Space Complexity: O(log n) average
isValidBST :: (Ord a) => BinaryTree a -> Bool
isValidBST tree = isValidBSTHelper tree Nothing Nothing
  where
    isValidBSTHelper Empty _ _ = True
    isValidBSTHelper (Node val left right) minVal maxVal =
      let validMin = maybe True (val >) minVal
          validMax = maybe True (val <) maxVal
      in validMin && validMax && 
         isValidBSTHelper left minVal (Just val) &&
         isValidBSTHelper right (Just val) maxVal

-- | Create a BST from a list
-- Time Complexity: O(n log n) average, O(n²) worst case
fromList :: (Ord a) => [a] -> BinaryTree a
fromList = foldr insertBST Empty

-- Example usage and main function
main :: IO ()
main = do
  -- Create a binary search tree
  let values = [5, 3, 7, 2, 4, 6, 8]
  let bst = fromList values
  
  putStrLn "Binary Search Tree Operations:"
  putStrLn $ "Original values: " ++ show values
  putStrLn $ "BST structure: " ++ show bst
  
  putStrLn "\nTraversals:"
  putStrLn $ "In-order: " ++ show (inOrderTraversal bst)
  putStrLn $ "Pre-order: " ++ show (preOrderTraversal bst)
  putStrLn $ "Post-order: " ++ show (postOrderTraversal bst)
  
  putStrLn "\nTree Properties:"
  putStrLn $ "Height: " ++ show (treeHeight bst)
  putStrLn $ "Node count: " ++ show (nodeCount bst)
  putStrLn $ "Minimum value: " ++ show (findMin bst)
  putStrLn $ "Maximum value: " ++ show (findMax bst)
  putStrLn $ "Is valid BST: " ++ show (isValidBST bst)
  
  putStrLn "\nSearch operations:"
  putStrLn $ "Search for 4: " ++ show (searchBST 4 bst)
  putStrLn $ "Search for 9: " ++ show (searchBST 9 bst)