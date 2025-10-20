#include <bits/stdc++.h>
using namespace std;

int peakIndexInMountainArray(const vector<int>& arr) {
    int left = 0, right = arr.size() - 1;
    while (left < right) {
        int mid = left + (right - left) / 2;
        if (arr[mid] < arr[mid + 1])
            left = mid + 1;
        else
            right = mid;
    }
    return left;
}

int main() {
    vector<int> arr = {24, 69, 100, 99, 79, 78, 67, 36, 26, 19};
    int peakIndex = peakIndexInMountainArray(arr);
    cout << "Peak index: " << peakIndex << endl;
    cout << "Peak element: " << arr[peakIndex] << endl;
    return 0;
}
