#include <iostream>
#include <vector>
#include <algorithm>
#include <sstream>

// Counting Sort Algorithm
// --------------------------------------------------------------------------------
// Description:
// Counting Sort is a non-comparison based integer sorting algorithm. 
// It is efficient when the range of input data (k) is not significantly 
// greater than the number of elements to be sorted (n).

// Constraints:
// This algorithm is typically used only for sorting non-negative integers.

// Time Complexity:
// Worst/Average/Best Case: O(n + k), where n is the number of elements and k 
// is the range of the non-negative input.

// Space Complexity:
// Auxiliary Space: O(k) - Additional space required for the count array.
// --------------------------------------------------------------------------------

void counting_sort(std::vector<int>& arr) {
    if (arr.empty()) {
        return;
    }

    // 1. Find the maximum element (k) to determine the size of the count array
    int max_val = *std::max_element(arr.begin(), arr.end());
    
    // 2. Initialize count array (size max_val + 1) and output array (size n)
    std::vector<int> count(max_val + 1, 0);
    std::vector<int> output(arr.size());

    // 3. Store the count of each element
    for (int x : arr) {
        count[x]++;
    }

    // 4. Modify the count array to store the actual position of elements in output array
    for (int i = 1; i <= max_val; i++) {
        count[i] += count[i - 1];
    }

    // 5. Build the output array (iterate backwards to maintain stability)
    for (int i = arr.size() - 1; i >= 0; i--) {
        output[count[arr[i]] - 1] = arr[i];
        count[arr[i]]--;
    }

    // 6. Copy the sorted elements back to the original array
    arr = output;
}

// Example Usage / Test Cases with User Input
int main() {
    std::vector<int> arr;
    std::string line;
    int number;

    // Prompt user for input
    std::cout << "Enter a list of non-negative integers separated by spaces: \n> ";
    
    // Read the entire line of input
    std::getline(std::cin, line);

    // Use stringstream to extract integers from the line
    std::stringstream ss(line);
    while (ss >> number) {
        if (number < 0) {
            std::cerr << "Error: Counting sort is for non-negative integers. Exiting.\n";
            return 1;
        }
        arr.push_back(number);
    }
    
    if (arr.empty()) {
        std::cout << "Input list was empty. Nothing to sort.\n";
        return 0;
    }

    // Display original list
    std::cout << "\nOriginal List: ";
    for (int x : arr) std::cout << x << " ";
    
    // Perform the sort
    counting_sort(arr);
    
    // Display sorted list
    std::cout << "\nSorted List:   ";
    for (int x : arr) std::cout << x << " ";

    std::cout << "\n";
    return 0;
}