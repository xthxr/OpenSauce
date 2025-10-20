using System;

/// <summary>
/// Singly Linked List with basic operations: InsertHead, InsertTail, Delete, Search, Print.
/// </summary>
public class SinglyLinkedList
{
    private class Node
    {
        public int Value;
        public Node Next;

        public Node(int value)
        {
            Value = value;
            Next = null;
        }
    }

    private Node head;
    private Node tail;
    private int count;

    // Time: O(1)
    public void InsertHead(int value)
    {
        Node node = new Node(value) { Next = head };
        head = node;
        if (tail == null) tail = head;
        count++;
    }

    // Time: O(1)
    public void InsertTail(int value)
    {
        Node node = new Node(value);
        if (tail == null)
        {
            head = tail = node;
        }
        else
        {
            tail.Next = node;
            tail = node;
        }
        count++;
    }

    // Time: O(n)
    public bool Delete(int value)
    {
        Node prev = null;
        Node curr = head;
        while (curr != null)
        {
            if (curr.Value == value)
            {
                if (prev == null)
                {
                    head = curr.Next;
                }
                else
                {
                    prev.Next = curr.Next;
                }
                if (curr == tail) tail = prev;
                count--;
                return true;
            }
            prev = curr;
            curr = curr.Next;
        }
        return false;
    }

    // Time: O(n)
    public bool Contains(int value)
    {
        for (Node curr = head; curr != null; curr = curr.Next)
        {
            if (curr.Value == value) return true;
        }
        return false;
    }

    // Time: O(n)
    public override string ToString()
    {
        System.Text.StringBuilder sb = new System.Text.StringBuilder();
        for (Node curr = head; curr != null; curr = curr.Next)
        {
            sb.Append(curr.Value);
            if (curr.Next != null) sb.Append(" -> ");
        }
        return sb.ToString();
    }

    public static void Main()
    {
        var list = new SinglyLinkedList();
        list.InsertHead(3);
        list.InsertHead(2);
        list.InsertTail(4);
        list.InsertHead(1);
        Console.WriteLine("[SinglyLinkedList] " + list); // 1 -> 2 -> 3 -> 4
        Console.WriteLine("Contains 3? " + list.Contains(3)); // True
        Console.WriteLine("Delete 2: " + list.Delete(2)); // True
        Console.WriteLine("[SinglyLinkedList] " + list); // 1 -> 3 -> 4
    }
}


