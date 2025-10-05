#include <bits/stdc++.h>
using namespace std;

/*
Problem: Binary Search
Topic: searching
Description: Given a sorted array and a target, find the index of the target using binary search.
Time Complexity: O(log n)
Space Complexity: O(1)
*/

int binarySearch(vector<int>& arr, int key){
    int low = 0, high = arr.size() - 1;
    while(low <= high){
        int mid = low + (high - low)/2;
        if(arr[mid] == key) return mid;
        else if(arr[mid] < key) low = mid + 1;
        else high = mid - 1;
    }
    return -1;
}

int main(){
    vector<int> arr = {1,3,5,7,9};
    int key = 7;
    int idx = binarySearch(arr,key);
    if(idx != -1) cout << "Element found at index: " << idx << "\n";
    else cout << "Element not found\n";
}
