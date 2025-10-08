/*

Bucket Sort is also a non-comparison-based sorting algorithm.
It works by distributing elements into multiple buckets, sorting each bucket individually (using another sorting algorithm like Insertion Sort), and then combining them.

Algorithm Steps:

Create empty buckets.

Scatter elements into buckets based on range.

Sort each bucket individually.

Concatenate all buckets to get the sorted array.

Time Complexity:

Best Case: O(n + k)

Average Case: O(n + k)

Worst Case: O(n²)

Space Complexity: O(n + k)

Stability: ✅ Stable

*/


#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

void bucketSort(float arr[], int n) {
    vector<vector<float>> buckets(n);


    // Put array elements into buckets
    for (int i = 0; i < n; i++) {
        int index = n * arr[i]; // Index in bucket
        buckets[index].push_back(arr[i]);
    }

    // Sort individual buckets
    for (int i = 0; i < n; i++)
        sort(buckets[i].begin(), buckets[i].end());

    // Concatenate all buckets into arr[]
    int idx = 0;
    for (int i = 0; i < n; i++)
        for (float val : buckets[i])
            arr[idx++] = val;
}

int main() {
    float arr[] = {0.897, 0.565, 0.656, 0.123, 0.665, 0.343};
    int n = sizeof(arr) / sizeof(arr[0]);

    bucketSort(arr, n);

    cout << "Sorted array: ";
    for (int i = 0; i < n; i++)
        cout << arr[i] << " ";

    return 0;
}
