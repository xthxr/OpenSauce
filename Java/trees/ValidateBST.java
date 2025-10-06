import java.util.Stack;
public class ValidateBST {
    public static class TreeNode {
        int val;
        TreeNode left;
        TreeNode right;
        
        TreeNode() {}
        TreeNode(int val) { this.val = val; }
        TreeNode(int val, TreeNode left, TreeNode right) {
            this.val = val;
            this.left = left;
            this.right = right;
        }
    }

    public boolean isValidBST(TreeNode root) {
        return validate(root, Long.MIN_VALUE, Long.MAX_VALUE);
    }

    private boolean validate(TreeNode node, long min, long max) {
        if (node == null) return true;
        
        // Check current node against boundaries
        if (node.val <= min || node.val >= max) {
            return false;
        }
        
        // Validate left and right subtrees with updated boundaries
        return validate(node.left, min, node.val) && 
               validate(node.right, node.val, max);
    }

    public boolean isValidBSTInorder(TreeNode root) {
        if (root == null) return true;
        
        Stack<TreeNode> stack = new Stack<>();
        TreeNode current = root;
        Integer prev = null;  // Tracks previous value in inorder traversal
        
        while (current != null || !stack.isEmpty()) {
            // Reach the leftmost node
            while (current != null) {
                stack.push(current);
                current = current.left;
            }
            
            current = stack.pop();
            
            // Check if current value is greater than previous
            if (prev != null && current.val <= prev) {
                return false;
            }
            
            prev = current.val;
            current = current.right;
        }
        
        return true;
    }

    public static void main(String[] args) {
        ValidateBST validator = new ValidateBST();
        
        // Test Case 1: Valid BST
        TreeNode root1 = new TreeNode(2, 
                            new TreeNode(1), 
                            new TreeNode(3));
        System.out.println("Test 1 - Valid BST: " + validator.isValidBST(root1));
        
        // Test Case 2: Invalid BST
        TreeNode root2 = new TreeNode(5,
                            new TreeNode(1),
                            new TreeNode(4, 
                                new TreeNode(3), 
                                new TreeNode(6)));
        System.out.println("Test 2 - Invalid BST: " + validator.isValidBST(root2));
        
        // Test Case 3: Single node
        TreeNode root3 = new TreeNode(1);
        System.out.println("Test 3 - Single node: " + validator.isValidBST(root3));
        
        // Test Case 4: Empty tree
        System.out.println("Test 4 - Empty tree: " + validator.isValidBST(null));
        
        // Test Case 5: Compare both methods
        System.out.println("Both methods agree on Test 1: " + 
            (validator.isValidBST(root1) == validator.isValidBSTInorder(root1)));
    }
}
