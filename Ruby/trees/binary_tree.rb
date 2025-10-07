# Binary Tree implementation in Ruby
# Includes tree construction, traversals, and common operations

# Binary Tree Node class
class TreeNode
  attr_accessor :value, :left, :right
  
  def initialize(value)
    @value = value
    @left = nil
    @right = nil
  end
  
  def to_s
    "Node(#{@value})"
  end
end

# Binary Search Tree class
class BinarySearchTree
  attr_accessor :root
  
  def initialize
    @root = nil
  end
  
  # Insert a value into the binary search tree
  # Time Complexity: O(log n) average, O(n) worst case
  # Space Complexity: O(log n) average, O(n) worst case (due to recursion)
  def insert(value)
    @root = insert_recursive(@root, value)
  end
  
  # Search for a value in binary search tree
  # Time Complexity: O(log n) average, O(n) worst case
  # Space Complexity: O(log n) average, O(n) worst case (due to recursion)
  def search(value)
    search_recursive(@root, value)
  end
  
  # In-order traversal (Left -> Root -> Right)
  # Time Complexity: O(n), Space Complexity: O(n)
  def in_order_traversal
    result = []
    in_order_recursive(@root, result)
    result
  end
  
  # Pre-order traversal (Root -> Left -> Right)
  # Time Complexity: O(n), Space Complexity: O(n)
  def pre_order_traversal
    result = []
    pre_order_recursive(@root, result)
    result
  end
  
  # Post-order traversal (Left -> Right -> Root)
  # Time Complexity: O(n), Space Complexity: O(n)
  def post_order_traversal
    result = []
    post_order_recursive(@root, result)
    result
  end
  
  # Level-order traversal (Breadth-First Search)
  # Time Complexity: O(n), Space Complexity: O(n)
  def level_order_traversal
    return [] if @root.nil?
    
    result = []
    queue = [@root]
    
    until queue.empty?
      node = queue.shift
      result << node.value
      
      queue << node.left if node.left
      queue << node.right if node.right
    end
    
    result
  end
  
  # Calculate the height of the tree
  # Time Complexity: O(n), Space Complexity: O(log n) average
  def height
    height_recursive(@root)
  end
  
  # Count total number of nodes
  # Time Complexity: O(n), Space Complexity: O(log n) average
  def node_count
    count_recursive(@root)
  end
  
  # Find minimum value in BST
  # Time Complexity: O(log n) average, O(n) worst case
  def find_min
    return nil if @root.nil?
    
    current = @root
    current = current.left while current.left
    current.value
  end
  
  # Find maximum value in BST
  # Time Complexity: O(log n) average, O(n) worst case
  def find_max
    return nil if @root.nil?
    
    current = @root
    current = current.right while current.right
    current.value
  end
  
  # Check if tree is a valid BST
  # Time Complexity: O(n), Space Complexity: O(log n) average
  def valid_bst?
    valid_bst_recursive(@root, nil, nil)
  end
  
  # Delete a node from BST
  # Time Complexity: O(log n) average, O(n) worst case
  def delete(value)
    @root = delete_recursive(@root, value)
  end
  
  # Create BST from array
  def self.from_array(arr)
    bst = new
    arr.each { |value| bst.insert(value) }
    bst
  end
  
  # Convert BST to sorted array (using in-order traversal)
  def to_array
    in_order_traversal
  end
  
  # Pretty print the tree structure
  def display
    display_recursive(@root, "", true)
  end
  
  private
  
  def insert_recursive(node, value)
    return TreeNode.new(value) if node.nil?
    
    if value <= node.value
      node.left = insert_recursive(node.left, value)
    else
      node.right = insert_recursive(node.right, value)
    end
    
    node
  end
  
  def search_recursive(node, value)
    return false if node.nil?
    return true if node.value == value
    
    if value < node.value
      search_recursive(node.left, value)
    else
      search_recursive(node.right, value)
    end
  end
  
  def in_order_recursive(node, result)
    return if node.nil?
    
    in_order_recursive(node.left, result)
    result << node.value
    in_order_recursive(node.right, result)
  end
  
  def pre_order_recursive(node, result)
    return if node.nil?
    
    result << node.value
    pre_order_recursive(node.left, result)
    pre_order_recursive(node.right, result)
  end
  
  def post_order_recursive(node, result)
    return if node.nil?
    
    post_order_recursive(node.left, result)
    post_order_recursive(node.right, result)
    result << node.value
  end
  
  def height_recursive(node)
    return 0 if node.nil?
    
    1 + [height_recursive(node.left), height_recursive(node.right)].max
  end
  
  def count_recursive(node)
    return 0 if node.nil?
    
    1 + count_recursive(node.left) + count_recursive(node.right)
  end
  
  def valid_bst_recursive(node, min_val, max_val)
    return true if node.nil?
    
    return false if (min_val && node.value <= min_val) || (max_val && node.value >= max_val)
    
    valid_bst_recursive(node.left, min_val, node.value) &&
      valid_bst_recursive(node.right, node.value, max_val)
  end
  
  def delete_recursive(node, value)
    return node if node.nil?
    
    if value < node.value
      node.left = delete_recursive(node.left, value)
    elsif value > node.value
      node.right = delete_recursive(node.right, value)
    else
      # Node to be deleted found
      # Case 1: No child or only right child
      return node.right if node.left.nil?
      # Case 2: Only left child
      return node.left if node.right.nil?
      
      # Case 3: Two children
      # Find inorder successor (minimum in right subtree)
      successor = find_min_node(node.right)
      node.value = successor.value
      node.right = delete_recursive(node.right, successor.value)
    end
    
    node
  end
  
  def find_min_node(node)
    current = node
    current = current.left while current.left
    current
  end
  
  def display_recursive(node, prefix, is_last)
    return if node.nil?
    
    puts prefix + (is_last ? "└── " : "├── ") + node.value.to_s
    
    children = [node.left, node.right].compact
    children.each_with_index do |child, index|
      is_last_child = (index == children.length - 1)
      new_prefix = prefix + (is_last ? "    " : "│   ")
      display_recursive(child, new_prefix, is_last_child)
    end
  end
end

# Example usage and testing
if __FILE__ == $0
  puts "Binary Search Tree Operations:"
  
  # Create a binary search tree
  values = [5, 3, 7, 2, 4, 6, 8]
  bst = BinarySearchTree.from_array(values)
  
  puts "Original values: #{values}"
  puts "BST structure:"
  bst.display
  
  puts "\nTraversals:"
  puts "In-order: #{bst.in_order_traversal}"
  puts "Pre-order: #{bst.pre_order_traversal}"
  puts "Post-order: #{bst.post_order_traversal}"
  puts "Level-order: #{bst.level_order_traversal}"
  
  puts "\nTree Properties:"
  puts "Height: #{bst.height}"
  puts "Node count: #{bst.node_count}"
  puts "Minimum value: #{bst.find_min}"
  puts "Maximum value: #{bst.find_max}"
  puts "Is valid BST: #{bst.valid_bst?}"
  
  puts "\nSearch operations:"
  puts "Search for 4: #{bst.search(4)}"
  puts "Search for 9: #{bst.search(9)}"
  
  puts "\nInserting value 1:"
  bst.insert(1)
  puts "New in-order traversal: #{bst.in_order_traversal}"
  
  puts "\nDeleting value 3:"
  bst.delete(3)
  puts "After deletion: #{bst.in_order_traversal}"
  puts "Updated tree structure:"
  bst.display
  
  puts "\nConverted to sorted array: #{bst.to_array}"
end