/*
 * Binary Search Tree (BST)
 * 
 * Time Complexity:
 *   - Insert: O(h)
 *   - Search: O(h)
 *   - Inorder Traversal: O(n)
 * Space Complexity: O(h) due to recursion stack
 * 
 * A Binary Search Tree is a hierarchical data structure in which
 * each node has at most two children. For every node:
 *   - The left child's value is less than the parent’s value
 *   - The right child's value is greater than the parent’s value
 * 
 * Operations:
 *   - insert(value): Adds a new node into the BST
 *   - search(value): Checks if a value exists in the tree
 *   - inorder(): Prints the values in sorted (ascending) order
 */

object BinarySearchTree {

    /** Node class representing a single node in the BST */
    case class Node(var value: Int, var left: Option[Node] = None, var right: Option[Node] = None)

    /** Binary Search Tree class */
    class BST {
        var root: Option[Node] = None

        /** Inserts a value into the BST */
        def insert(value: Int): Unit = {
            root = insertRec(root, value)
        }

        private def insertRec(node: Option[Node], value: Int): Option[Node] = node match {
            case None => Some(Node(value))
            case Some(n) if value < n.value =>
                n.left = insertRec(n.left, value)
                Some(n)
            case Some(n) if value > n.value =>
                n.right = insertRec(n.right, value)
                Some(n)
            case _ => node // Duplicate values ignored
        }

        /** Searches for a value in the BST */
        def search(value: Int): Boolean = {
            def searchRec(node: Option[Node]): Boolean = node match {
                case None => false
                case Some(n) if n.value == value => true
                case Some(n) if value < n.value => searchRec(n.left)
                case Some(n) => searchRec(n.right)
            }
            searchRec(root)
        }

        /** Prints Binary Search Tree (BST) values in sorted (inorder) order */
        def inorder(): Unit = {
            def inorderRec(node: Option[Node]): Unit = node match {
                case Some(n) =>
                    inorderRec(n.left)
                    print(s"${n.value} ")
                    inorderRec(n.right)
                case None =>
            }
            inorderRec(root)
            println()
        }
    }

    /** Main method with example */
    def main(args: Array[String]): Unit = {
        val bst = new BST
        bst.insert(8)
        bst.insert(3)
        bst.insert(10)
        bst.insert(1)
        bst.insert(6)

        println("Inorder traversal:")
        bst.inorder() // Expected output: 1 3 6 8 10

        println(s"Search 6: ${bst.search(6)}") // Expected: true
        println(s"Search 9: ${bst.search(9)}") // Expected: false
    }
}
