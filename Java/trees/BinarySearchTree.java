public class BinarySearchTree {
    
    class Node {
        int key;
        Node left, right;

        public Node(int item) {
            key = item;
            left = right = null;
        }
    }

    // Root of the BST
    Node root;

    // Constructor to initialize an empty BST
    public BinarySearchTree() {
        root = null;
    }

    public void insert(int key) {
        root = insertRec(root, key);
    }

    private Node insertRec(Node root, int key) {
        // If the tree is empty, return a new node
        if (root == null) {
            root = new Node(key);
            return root;
        }

        // Otherwise, recur down the tree
        if (key < root.key) {
            root.left = insertRec(root.left, key);
        } else if (key > root.key) {
            root.right = insertRec(root.right, key);
        }

        // return the (unchanged) root pointer
        return root;
    }

    
    public boolean search(int key) {
        return searchRec(root, key);
    }
  
    private boolean searchRec(Node root, int key) {
        // Base Cases: root is null or key is present at root
        if (root == null) {
            return false;
        }
        if (root.key == key) {
            return true;
        }

        // Key is greater than root's key
        if (key > root.key) {
            return searchRec(root.right, key);
        }
        // Key is smaller than root's key
        return searchRec(root.left, key);
    }
    public void inOrder() {
        inOrderRec(root);
        System.out.println(); // Move to next line after traversal
    }


    private void inOrderRec(Node root) {
        if (root != null) {
            inOrderRec(root.left);
            System.out.print(root.key + " ");
            inOrderRec(root.right);
        }
    }

    public static void main(String[] args) {
        BinarySearchTree bst = new BinarySearchTree();

        // Insert nodes
        bst.insert(50);
        bst.insert(30);
        bst.insert(20);
        bst.insert(40);
        bst.insert(70);
        bst.insert(60);
        bst.insert(80);

        // Print in-order traversal of the BST
        System.out.print("In-order traversal of the BST: ");
        bst.inOrder(); // This should print: 20 30 40 50 60 70 80

        // Search for values
        System.out.println("Search for 40: " + (bst.search(40) ? "Found" : "Not Found"));
        System.out.println("Search for 90: " + (bst.search(90) ? "Found" : "Not Found"));
        System.out.println("Search for 20: " + (bst.search(20) ? "Found" : "Not Found"));
    }
}
