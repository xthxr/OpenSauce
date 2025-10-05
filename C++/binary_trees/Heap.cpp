#include <bits/stdc++.h>
using namespace std;

class maxHeap
{
    vector<int> arr;
    int size;

public:
    maxHeap() : size(0) {};

    void sift_up(int i)
    {
        if (i == 0)
        {
            return;
        }
        int parent = (i - 1) / 2;
        if (arr[parent] < arr[i])
        {
            swap(arr[parent], arr[i]);
            sift_up(parent);
        }
    }

    void insert(int x)
    {
        arr.push_back(x);
        size++;
        sift_up(size - 1);
    }

    void sift_down(int i)
    {
        int left = 2 * i + 1;
        int right = 2 * i + 2;
        int largest = i;
        if (left < size && arr[left] >= arr[largest])
        {
            largest = left;
        }
        if (right < size && arr[right] >= arr[largest])
        {
            largest = right;
        }
        if (largest != i)
        {
            swap(arr[i], arr[largest]);
            sift_down(largest);
        }
    }

    int extract()
    {
        if (arr.size() == 0)
            return -1;
        int value = arr[0];
        size--;
        arr[0] = arr[size];
        if (size > 0)
            sift_down(0);
        return value;
    }

    void display()
    {
        for (int i = 0; i < size; i++)
        {
            cout << arr[i] << " ";
        }
    }
};

class minHeap
{
private:
    vector<int> arr;
    int size;

public:
    minHeap() : size(0) {};

    void sift_up(int i)
    {
        if (i == 0)
            return;
        int parent = (i - 1) / 2;
        if (arr[parent] > arr[i])
        {
            swap(arr[parent], arr[i]);
            sift_up(parent);
        }
    }

    void insert(int val)
    {
        arr.push_back(val);
        size++;
        sift_up(size - 1);
    }

    void sift_down(int i)
    {
        int left = 2 * i + 1;
        int right = 2 * i + 2;
        int smallest = i;
        if (left < size && arr[left] <= arr[smallest])
        {
            smallest = left;
        }
        if (right < size && arr[right] <= arr[smallest])
        {
            smallest = right;
        }
        if (smallest != i)
        {
            swap(arr[smallest], arr[i]);
            sift_down(smallest);
        }
    }

    int extract()
    {
        if (size == 0)
            return -1;

        int num = arr[0];
        arr[0] = arr[size - 1];
        size--;
        if (size > 0)
            sift_down(0);
        return num;
    }

    void display()
    {
        for (int i = 0; i < size; i++)
        {
            cout << arr[i] << " ";
        }
        cout << '\n';
    }
};

int main()
{
    minHeap m1;
    m1.insert(4);
    m1.insert(37);
    m1.insert(23);
    m1.insert(12);
    m1.insert(60);
    cout << m1.extract() << endl;
    m1.insert(84);
    m1.insert(29);
    m1.insert(100);
    m1.insert(71);
    cout << m1.extract() << endl;
    m1.display();
}