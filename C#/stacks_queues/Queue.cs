using System;

/// <summary>
/// Circular array-backed queue implementation for integers.
/// </summary>
public class Queue
{
    private int[] data;
    private int head; // points to current front element
    private int tail; // points to next insertion index
    private int size;

    public Queue(int capacity = 16)
    {
        data = new int[Math.Max(1, capacity)];
        head = 0;
        tail = 0;
        size = 0;
    }

    // Time: O(1) amortized
    public void Enqueue(int value)
    {
        if (size == data.Length)
        {
            Resize(data.Length * 2);
        }
        data[tail] = value;
        tail = (tail + 1) % data.Length;
        size++;
    }

    // Time: O(1)
    public int Dequeue()
    {
        if (IsEmpty()) throw new InvalidOperationException("Queue is empty");
        int value = data[head];
        head = (head + 1) % data.Length;
        size--;
        return value;
    }

    public bool IsEmpty() => size == 0;

    public int Count => size;

    private void Resize(int newCapacity)
    {
        int[] newData = new int[newCapacity];
        for (int i = 0; i < size; i++)
        {
            newData[i] = data[(head + i) % data.Length];
        }
        data = newData;
        head = 0;
        tail = size;
    }

    public static void Main()
    {
        var q = new Queue();
        q.Enqueue(1);
        q.Enqueue(2);
        q.Enqueue(3);
        Console.WriteLine("[Queue] Dequeue: " + q.Dequeue()); // 1
        Console.WriteLine("[Queue] Count: " + q.Count); // 2
    }
}


