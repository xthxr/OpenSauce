/// Binary Tree implementation in Dart
/// Includes tree construction, traversals, and common operations

/// Binary Tree node class
class BinaryTreeNode<T> {
  T value;
  BinaryTreeNode<T>? left;
  BinaryTreeNode<T>? right;
  
  BinaryTreeNode(this.value, [this.left, this.right]);
  
  @override
  String toString() => 'Node($value)';
}

/// Binary Tree class with various operations
class BinaryTree<T extends Comparable<T>> {
  BinaryTreeNode<T>? root;
  
  BinaryTree([this.root]);
  
  /// Insert a value into a binary search tree
  /// Time Complexity: O(log n) average, O(n) worst case
  /// Space Complexity: O(log n) average, O(n) worst case
  void insertBST(T value) {
    root = _insertBSTHelper(root, value);
  }
  
  BinaryTreeNode<T> _insertBSTHelper(BinaryTreeNode<T>? node, T value) {
    if (node == null) return BinaryTreeNode<T>(value);
    
    if (value.compareTo(node.value) <= 0) {
      node.left = _insertBSTHelper(node.left, value);
    } else {
      node.right = _insertBSTHelper(node.right, value);
    }
    
    return node;
  }
  
  /// Search for a value in binary search tree
  /// Time Complexity: O(log n) average, O(n) worst case
  /// Space Complexity: O(log n) average, O(n) worst case
  bool searchBST(T value) {
    return _searchBSTHelper(root, value);
  }
  
  bool _searchBSTHelper(BinaryTreeNode<T>? node, T value) {
    if (node == null) return false;
    
    if (value == node.value) return true;
    if (value.compareTo(node.value) < 0) {
      return _searchBSTHelper(node.left, value);
    } else {
      return _searchBSTHelper(node.right, value);
    }
  }
  
  /// In-order traversal (Left -> Root -> Right)
  /// Time Complexity: O(n), Space Complexity: O(n)
  List<T> inOrderTraversal() {
    List<T> result = [];
    _inOrderHelper(root, result);
    return result;
  }
  
  void _inOrderHelper(BinaryTreeNode<T>? node, List<T> result) {
    if (node != null) {
      _inOrderHelper(node.left, result);
      result.add(node.value);
      _inOrderHelper(node.right, result);
    }
  }
  
  /// Pre-order traversal (Root -> Left -> Right)
  /// Time Complexity: O(n), Space Complexity: O(n)
  List<T> preOrderTraversal() {
    List<T> result = [];
    _preOrderHelper(root, result);
    return result;
  }
  
  void _preOrderHelper(BinaryTreeNode<T>? node, List<T> result) {
    if (node != null) {
      result.add(node.value);
      _preOrderHelper(node.left, result);
      _preOrderHelper(node.right, result);
    }
  }
  
  /// Post-order traversal (Left -> Right -> Root)
  /// Time Complexity: O(n), Space Complexity: O(n)
  List<T> postOrderTraversal() {
    List<T> result = [];
    _postOrderHelper(root, result);
    return result;
  }
  
  void _postOrderHelper(BinaryTreeNode<T>? node, List<T> result) {
    if (node != null) {
      _postOrderHelper(node.left, result);
      _postOrderHelper(node.right, result);
      result.add(node.value);
    }
  }
  
  /// Level-order traversal (Breadth-First Search)
  /// Time Complexity: O(n), Space Complexity: O(n)
  List<T> levelOrderTraversal() {
    if (root == null) return [];
    
    List<T> result = [];
    List<BinaryTreeNode<T>> queue = [root!];
    
    while (queue.isNotEmpty) {
      BinaryTreeNode<T> current = queue.removeAt(0);
      result.add(current.value);
      
      if (current.left != null) queue.add(current.left!);
      if (current.right != null) queue.add(current.right!);
    }
    
    return result;
  }
  
  /// Calculate the height of the tree
  /// Time Complexity: O(n), Space Complexity: O(log n) average
  int treeHeight() {
    return _heightHelper(root);
  }
  
  int _heightHelper(BinaryTreeNode<T>? node) {
    if (node == null) return 0;
    return 1 + [_heightHelper(node.left), _heightHelper(node.right)].reduce((a, b) => a > b ? a : b);
  }
  
  /// Count total number of nodes
  /// Time Complexity: O(n), Space Complexity: O(log n) average
  int nodeCount() {
    return _countHelper(root);
  }
  
  int _countHelper(BinaryTreeNode<T>? node) {
    if (node == null) return 0;
    return 1 + _countHelper(node.left) + _countHelper(node.right);
  }
  
  /// Find minimum value in BST
  /// Time Complexity: O(log n) average, O(n) worst case
  T? findMin() {
    if (root == null) return null;
    return _findMinHelper(root!);
  }
  
  T _findMinHelper(BinaryTreeNode<T> node) {
    while (node.left != null) {
      node = node.left!;
    }
    return node.value;
  }
  
  /// Find maximum value in BST
  /// Time Complexity: O(log n) average, O(n) worst case
  T? findMax() {
    if (root == null) return null;
    return _findMaxHelper(root!);
  }
  
  T _findMaxHelper(BinaryTreeNode<T> node) {
    while (node.right != null) {
      node = node.right!;
    }
    return node.value;
  }
  
  /// Check if tree is a valid BST
  /// Time Complexity: O(n), Space Complexity: O(log n) average
  bool isValidBST() {
    return _isValidBSTHelper(root, null, null);
  }
  
  bool _isValidBSTHelper(BinaryTreeNode<T>? node, T? minVal, T? maxVal) {
    if (node == null) return true;
    
    if ((minVal != null && node.value.compareTo(minVal) <= 0) ||
        (maxVal != null && node.value.compareTo(maxVal) >= 0)) {
      return false;
    }
    
    return _isValidBSTHelper(node.left, minVal, node.value) &&
           _isValidBSTHelper(node.right, node.value, maxVal);
  }
  
  /// Delete a node from BST
  /// Time Complexity: O(log n) average, O(n) worst case
  void deleteBST(T value) {
    root = _deleteBSTHelper(root, value);
  }
  
  BinaryTreeNode<T>? _deleteBSTHelper(BinaryTreeNode<T>? node, T value) {
    if (node == null) return null;
    
    if (value.compareTo(node.value) < 0) {
      node.left = _deleteBSTHelper(node.left, value);
    } else if (value.compareTo(node.value) > 0) {
      node.right = _deleteBSTHelper(node.right, value);
    } else {
      // Node to be deleted found
      if (node.left == null) return node.right;
      if (node.right == null) return node.left;
      
      // Node has two children
      T minVal = _findMinHelper(node.right!);
      node.value = minVal;
      node.right = _deleteBSTHelper(node.right, minVal);
    }
    
    return node;
  }
  
  /// Create a BST from a list
  /// Time Complexity: O(n log n) average, O(n²) worst case
  static BinaryTree<T> fromList<T extends Comparable<T>>(List<T> values) {
    BinaryTree<T> tree = BinaryTree<T>();
    for (T value in values) {
      tree.insertBST(value);
    }
    return tree;
  }
  
  /// Check if tree is balanced
  /// Time Complexity: O(n), Space Complexity: O(log n) average
  bool isBalanced() {
    return _isBalancedHelper(root) != -1;
  }
  
  int _isBalancedHelper(BinaryTreeNode<T>? node) {
    if (node == null) return 0;
    
    int leftHeight = _isBalancedHelper(node.left);
    if (leftHeight == -1) return -1;
    
    int rightHeight = _isBalancedHelper(node.right);
    if (rightHeight == -1) return -1;
    
    if ((leftHeight - rightHeight).abs() > 1) return -1;
    
    return [leftHeight, rightHeight].reduce((a, b) => a > b ? a : b) + 1;
  }
}

void main() {
  // Create a binary search tree
  List<int> values = [5, 3, 7, 2, 4, 6, 8];
  BinaryTree<int> bst = BinaryTree.fromList(values);
  
  print('Binary Search Tree Examples:');
  print('Created BST from values: $values');
  print('In-order traversal: ${bst.inOrderTraversal()}');
  print('Pre-order traversal: ${bst.preOrderTraversal()}');
  print('Post-order traversal: ${bst.postOrderTraversal()}');
  print('Level-order traversal: ${bst.levelOrderTraversal()}');
  
  print('\nTree properties:');
  print('Height: ${bst.treeHeight()}');
  print('Node count: ${bst.nodeCount()}');
  print('Minimum value: ${bst.findMin()}');
  print('Maximum value: ${bst.findMax()}');
  print('Is valid BST: ${bst.isValidBST()}');
  print('Is balanced: ${bst.isBalanced()}');
  
  print('\nSearch operations:');
  print('Search for 4: ${bst.searchBST(4)}');
  print('Search for 9: ${bst.searchBST(9)}');
  
  print('\nDeletion:');
  print('Before deletion: ${bst.inOrderTraversal()}');
  bst.deleteBST(3);
  print('After deleting 3: ${bst.inOrderTraversal()}');
  
  // Test with strings
  BinaryTree<String> stringTree = BinaryTree<String>();
  List<String> words = ['dog', 'cat', 'elephant', 'ant', 'bird'];
  for (String word in words) {
    stringTree.insertBST(word);
  }
  
  print('\nString BST:');
  print('Words: $words');
  print('In-order: ${stringTree.inOrderTraversal()}');
}