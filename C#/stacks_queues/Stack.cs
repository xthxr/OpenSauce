using System;

/// <summary>
/// Array-backed stack implementation for integers.
/// </summary>
public class Stack
{
    private int[] data;
    private int top;

    public Stack(int capacity = 16)
    {
        data = new int[Math.Max(1, capacity)];
        top = -1;
    }

    // Time: O(1) amortized, Space: O(n)
    public void Push(int value)
    {
        if (top + 1 == data.Length)
        {
            Array.Resize(ref data, data.Length * 2);
        }
        data[++top] = value;
    }

    // Time: O(1)
    public int Pop()
    {
        if (IsEmpty()) throw new InvalidOperationException("Stack is empty");
        return data[top--];
    }

    // Time: O(1)
    public int Peek()
    {
        if (IsEmpty()) throw new InvalidOperationException("Stack is empty");
        return data[top];
    }

    public bool IsEmpty() => top < 0;

    public int Count => top + 1;

    public static void Main()
    {
        var s = new Stack();
        s.Push(10);
        s.Push(20);
        Console.WriteLine("[Stack] Peek: " + s.Peek()); // 20
        Console.WriteLine("[Stack] Pop: " + s.Pop()); // 20
        Console.WriteLine("[Stack] Count: " + s.Count); // 1
    }
}


