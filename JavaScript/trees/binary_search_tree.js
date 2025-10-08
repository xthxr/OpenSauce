/**
 * Binary Search Tree Implementation
 * 
 * Time Complexity: 
 *   - Insert: O(log n) average, O(n) worst case
 *   - Search: O(log n) average, O(n) worst case
 *   - Delete: O(log n) average, O(n) worst case
 * Space Complexity: O(n) for storing n nodes
 * 
 * A Binary Search Tree (BST) is a binary tree where each node has at most two children,
 * and for each node, all values in the left subtree are less than the node's value,
 * and all values in the right subtree are greater than the node's value.
 */

class TreeNode {
    constructor(value) {
        this.value = value;
        this.left = null;
        this.right = null;
    }
}

class BinarySearchTree {
    constructor() {
        this.root = null;
    }

    /**
     * Insert a new value into the BST
     * 
     * @param {number} value - Value to insert
     * @returns {boolean} - True if inserted successfully
     * 
     * @example
     * const bst = new BinarySearchTree();
     * bst.insert(5);
     * bst.insert(3);
     * bst.insert(7);
     */
    insert(value) {
        const newNode = new TreeNode(value);
        
        if (this.root === null) {
            this.root = newNode;
            return true;
        }
        
        return this.insertNode(this.root, newNode);
    }

    insertNode(node, newNode) {
        if (newNode.value < node.value) {
            if (node.left === null) {
                node.left = newNode;
                return true;
            } else {
                return this.insertNode(node.left, newNode);
            }
        } else if (newNode.value > node.value) {
            if (node.right === null) {
                node.right = newNode;
                return true;
            } else {
                return this.insertNode(node.right, newNode);
            }
        } else {
            // Value already exists
            return false;
        }
    }

    /**
     * Search for a value in the BST
     * 
     * @param {number} value - Value to search for
     * @returns {boolean} - True if value exists
     * 
     * @example
     * bst.search(5); // returns true if 5 exists
     */
    search(value) {
        return this.searchNode(this.root, value);
    }

    searchNode(node, value) {
        if (node === null) {
            return false;
        }
        
        if (value < node.value) {
            return this.searchNode(node.left, value);
        } else if (value > node.value) {
            return this.searchNode(node.right, value);
        } else {
            return true;
        }
    }

    /**
     * Delete a value from the BST
     * 
     * @param {number} value - Value to delete
     * @returns {boolean} - True if deleted successfully
     * 
     * @example
     * bst.delete(5); // removes 5 from the tree
     */
    delete(value) {
        this.root = this.deleteNode(this.root, value);
        return this.root !== null;
    }

    deleteNode(node, value) {
        if (node === null) {
            return null;
        }
        
        if (value < node.value) {
            node.left = this.deleteNode(node.left, value);
        } else if (value > node.value) {
            node.right = this.deleteNode(node.right, value);
        } else {
            // Node to be deleted found
            
            // Case 1: Node with no children (leaf node)
            if (node.left === null && node.right === null) {
                return null;
            }
            
            // Case 2: Node with one child
            if (node.left === null) {
                return node.right;
            }
            if (node.right === null) {
                return node.left;
            }
            
            // Case 3: Node with two children
            // Find the inorder successor (smallest in right subtree)
            const successor = this.findMin(node.right);
            node.value = successor.value;
            node.right = this.deleteNode(node.right, successor.value);
        }
        
        return node;
    }

    /**
     * Find the minimum value in the BST
     * 
     * @returns {number|null} - Minimum value or null if tree is empty
     */
    findMin(node = this.root) {
        if (node === null) {
            return null;
        }
        
        while (node.left !== null) {
            node = node.left;
        }
        
        return node;
    }

    /**
     * Find the maximum value in the BST
     * 
     * @returns {number|null} - Maximum value or null if tree is empty
     */
    findMax(node = this.root) {
        if (node === null) {
            return null;
        }
        
        while (node.right !== null) {
            node = node.right;
        }
        
        return node;
    }

    /**
     * Inorder traversal (Left, Root, Right) - gives sorted order
     * 
     * @returns {number[]} - Array of values in sorted order
     */
    inorderTraversal() {
        const result = [];
        this.inorderHelper(this.root, result);
        return result;
    }

    inorderHelper(node, result) {
        if (node !== null) {
            this.inorderHelper(node.left, result);
            result.push(node.value);
            this.inorderHelper(node.right, result);
        }
    }

    /**
     * Preorder traversal (Root, Left, Right)
     * 
     * @returns {number[]} - Array of values in preorder
     */
    preorderTraversal() {
        const result = [];
        this.preorderHelper(this.root, result);
        return result;
    }

    preorderHelper(node, result) {
        if (node !== null) {
            result.push(node.value);
            this.preorderHelper(node.left, result);
            this.preorderHelper(node.right, result);
        }
    }

    /**
     * Postorder traversal (Left, Right, Root)
     * 
     * @returns {number[]} - Array of values in postorder
     */
    postorderTraversal() {
        const result = [];
        this.postorderHelper(this.root, result);
        return result;
    }

    postorderHelper(node, result) {
        if (node !== null) {
            this.postorderHelper(node.left, result);
            this.postorderHelper(node.right, result);
            result.push(node.value);
        }
    }

    /**
     * Get the height of the BST
     * 
     * @returns {number} - Height of the tree
     */
    getHeight() {
        return this.getHeightHelper(this.root);
    }

    getHeightHelper(node) {
        if (node === null) {
            return -1;
        }
        
        const leftHeight = this.getHeightHelper(node.left);
        const rightHeight = this.getHeightHelper(node.right);
        
        return Math.max(leftHeight, rightHeight) + 1;
    }

    /**
     * Check if the BST is valid
     * 
     * @returns {boolean} - True if valid BST
     */
    isValidBST() {
        return this.isValidBSTHelper(this.root, Number.MIN_SAFE_INTEGER, Number.MAX_SAFE_INTEGER);
    }

    isValidBSTHelper(node, min, max) {
        if (node === null) {
            return true;
        }
        
        if (node.value <= min || node.value >= max) {
            return false;
        }
        
        return this.isValidBSTHelper(node.left, min, node.value) &&
               this.isValidBSTHelper(node.right, node.value, max);
    }

    /**
     * Get the size (number of nodes) of the BST
     * 
     * @returns {number} - Number of nodes in the tree
     */
    getSize() {
        return this.getSizeHelper(this.root);
    }

    getSizeHelper(node) {
        if (node === null) {
            return 0;
        }
        
        return 1 + this.getSizeHelper(node.left) + this.getSizeHelper(node.right);
    }

    /**
     * Check if the BST is empty
     * 
     * @returns {boolean} - True if tree is empty
     */
    isEmpty() {
        return this.root === null;
    }

    /**
     * Clear the BST
     */
    clear() {
        this.root = null;
    }
}

// Test cases and examples
function testBinarySearchTree() {
    console.log("Binary Search Tree Test Cases");
    console.log("=============================");
    
    const bst = new BinarySearchTree();
    
    // Test insertion
    console.log("\n1. Testing Insertion:");
    const values = [50, 30, 70, 20, 40, 60, 80, 10, 25, 35, 45];
    values.forEach(value => {
        const inserted = bst.insert(value);
        console.log(`Inserted ${value}: ${inserted ? 'Success' : 'Failed (duplicate)'}`);
    });
    
    // Test search
    console.log("\n2. Testing Search:");
    const searchValues = [50, 25, 100, 35];
    searchValues.forEach(value => {
        const found = bst.search(value);
        console.log(`Search ${value}: ${found ? 'Found' : 'Not found'}`);
    });
    
    // Test traversals
    console.log("\n3. Testing Traversals:");
    console.log(`Inorder (sorted): ${bst.inorderTraversal()}`);
    console.log(`Preorder: ${bst.preorderTraversal()}`);
    console.log(`Postorder: ${bst.postorderTraversal()}`);
    
    // Test min/max
    console.log("\n4. Testing Min/Max:");
    console.log(`Minimum value: ${bst.findMin()?.value || 'Tree is empty'}`);
    console.log(`Maximum value: ${bst.findMax()?.value || 'Tree is empty'}`);
    
    // Test tree properties
    console.log("\n5. Testing Tree Properties:");
    console.log(`Height: ${bst.getHeight()}`);
    console.log(`Size: ${bst.getSize()}`);
    console.log(`Is valid BST: ${bst.isValidBST()}`);
    console.log(`Is empty: ${bst.isEmpty()}`);
    
    // Test deletion
    console.log("\n6. Testing Deletion:");
    const deleteValues = [20, 70, 50];
    deleteValues.forEach(value => {
        const deleted = bst.delete(value);
        console.log(`Deleted ${value}: ${deleted ? 'Success' : 'Failed'}`);
        console.log(`Inorder after deletion: ${bst.inorderTraversal()}`);
    });
    
    // Test edge cases
    console.log("\n7. Testing Edge Cases:");
    const emptyBST = new BinarySearchTree();
    console.log(`Empty tree height: ${emptyBST.getHeight()}`);
    console.log(`Empty tree size: ${emptyBST.getSize()}`);
    console.log(`Empty tree is empty: ${emptyBST.isEmpty()}`);
    
    // Test with single node
    emptyBST.insert(42);
    console.log(`Single node tree height: ${emptyBST.getHeight()}`);
    console.log(`Single node tree size: ${emptyBST.getSize()}`);
}

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = {
        BinarySearchTree,
        TreeNode,
        testBinarySearchTree
    };
}

// Run tests if this file is executed directly
if (typeof window === 'undefined' && require.main === module) {
    testBinarySearchTree();
}
