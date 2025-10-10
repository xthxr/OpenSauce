class TreeNode {
    int val;
    TreeNode left, right;
    TreeNode(int x) { val = x; }
}
public class LowestCommonAncestor {
    /**
     * Recursive function to find LCA of two nodes n1 and n2.
     */
    public static TreeNode lca(TreeNode root, int n1, int n2) {
        if (root == null) return null;
        if (root.val == n1 || root.val == n2)
            return root;

        TreeNode left = lca(root.left, n1, n2);
        TreeNode right = lca(root.right, n1, n2);

        if (left != null && right != null)
            return root;
        return left != null ? left : right;
    }
    //Example Usage
    public static void main(String[] args) {
    // Build example binary tree
    TreeNode root = new TreeNode(3);
    root.left = new TreeNode(5);
    root.right = new TreeNode(1);
    root.left.left = new TreeNode(6);
    root.left.right = new TreeNode(2);
    root.right.left = new TreeNode(0);
    root.right.right = new TreeNode(8);

    TreeNode ancestor = LowestCommonAncestor.lca(root, 5, 1);
    System.out.println("LCA is: " + ancestor.val); // Output: 3

    ancestor = LowestCommonAncestor.lca(root, 6, 2);
    System.out.println("LCA is: " + ancestor.val); // Output: 5
}
}

