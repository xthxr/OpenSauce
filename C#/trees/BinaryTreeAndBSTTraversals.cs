using System;
using System.Collections.Generic;

/// <summary>
/// Binary Tree node and traversals (DFS: Preorder, Inorder, Postorder) and BFS (Level Order).
/// Includes simple BST insert to build example trees.
/// </summary>
public static class BinaryTreeAndBSTTraversals
{
    public class Node
    {
        public int Value;
        public Node Left;
        public Node Right;
        public Node(int value) { Value = value; }
    }

    // Time: O(h) average for balanced trees; Space: O(1) extra aside recursion
    public static Node BstInsert(Node root, int value)
    {
        if (root == null) return new Node(value);
        if (value < root.Value) root.Left = BstInsert(root.Left, value);
        else root.Right = BstInsert(root.Right, value);
        return root;
    }

    // Time: O(n), Space: O(h)
    public static void Preorder(Node root, List<int> output)
    {
        if (root == null) return;
        output.Add(root.Value);
        Preorder(root.Left, output);
        Preorder(root.Right, output);
    }

    // Time: O(n), Space: O(h)
    public static void Inorder(Node root, List<int> output)
    {
        if (root == null) return;
        Inorder(root.Left, output);
        output.Add(root.Value);
        Inorder(root.Right, output);
    }

    // Time: O(n), Space: O(h)
    public static void Postorder(Node root, List<int> output)
    {
        if (root == null) return;
        Postorder(root.Left, output);
        Postorder(root.Right, output);
        output.Add(root.Value);
    }

    // Time: O(n), Space: O(w) where w is max width
    public static void LevelOrder(Node root, List<int> output)
    {
        if (root == null) return;
        var q = new System.Collections.Generic.Queue<Node>();
        q.Enqueue(root);
        while (q.Count > 0)
        {
            var node = q.Dequeue();
            output.Add(node.Value);
            if (node.Left != null) q.Enqueue(node.Left);
            if (node.Right != null) q.Enqueue(node.Right);
        }
    }

    public static void Main()
    {
        Node root = null;
        foreach (var v in new[] { 5, 3, 7, 2, 4, 6, 8 }) root = BstInsert(root, v);

        var pre = new List<int>();
        var ino = new List<int>();
        var post = new List<int>();
        var level = new List<int>();

        Preorder(root, pre);
        Inorder(root, ino);
        Postorder(root, post);
        LevelOrder(root, level);

        Console.WriteLine("[Tree] Preorder:  " + string.Join(", ", pre));
        Console.WriteLine("[Tree] Inorder:   " + string.Join(", ", ino));
        Console.WriteLine("[Tree] Postorder: " + string.Join(", ", post));
        Console.WriteLine("[Tree] Level:     " + string.Join(", ", level));
    }
}


