/// Binary search implementation in Dart
/// Time Complexity: O(log n)
/// Space Complexity: O(log n) due to recursion

/// Binary search function that returns the index of target element
/// Returns null if element is not found
int? binarySearch(List<int> arr, int target) {
  if (arr.isEmpty) return null;
  return _binarySearchHelper(arr, target, 0, arr.length - 1);
}

/// Helper function for binary search with bounds
int? _binarySearchHelper(List<int> arr, int target, int low, int high) {
  if (low > high) return null;
  
  int mid = (low + high) ~/ 2;
  
  if (arr[mid] == target) {
    return mid;
  } else if (arr[mid] > target) {
    return _binarySearchHelper(arr, target, low, mid - 1);
  } else {
    return _binarySearchHelper(arr, target, mid + 1, high);
  }
}

/// Iterative binary search implementation
/// Time Complexity: O(log n)
/// Space Complexity: O(1)
int? binarySearchIterative(List<int> arr, int target) {
  if (arr.isEmpty) return null;
  
  int low = 0;
  int high = arr.length - 1;
  
  while (low <= high) {
    int mid = (low + high) ~/ 2;
    
    if (arr[mid] == target) {
      return mid;
    } else if (arr[mid] > target) {
      high = mid - 1;
    } else {
      low = mid + 1;
    }
  }
  
  return null;
}

/// Binary search that finds the leftmost occurrence of target
/// Returns null if element is not found
int? binarySearchLeftmost(List<int> arr, int target) {
  if (arr.isEmpty) return null;
  
  int low = 0;
  int high = arr.length - 1;
  int? result;
  
  while (low <= high) {
    int mid = (low + high) ~/ 2;
    
    if (arr[mid] == target) {
      result = mid;
      high = mid - 1; // Continue searching in the left half
    } else if (arr[mid] > target) {
      high = mid - 1;
    } else {
      low = mid + 1;
    }
  }
  
  return result;
}

/// Binary search that finds the rightmost occurrence of target
/// Returns null if element is not found
int? binarySearchRightmost(List<int> arr, int target) {
  if (arr.isEmpty) return null;
  
  int low = 0;
  int high = arr.length - 1;
  int? result;
  
  while (low <= high) {
    int mid = (low + high) ~/ 2;
    
    if (arr[mid] == target) {
      result = mid;
      low = mid + 1; // Continue searching in the right half
    } else if (arr[mid] > target) {
      high = mid - 1;
    } else {
      low = mid + 1;
    }
  }
  
  return result;
}

/// Generic binary search that works with any comparable type
int? binarySearchGeneric<T extends Comparable<T>>(List<T> arr, T target) {
  if (arr.isEmpty) return null;
  
  int low = 0;
  int high = arr.length - 1;
  
  while (low <= high) {
    int mid = (low + high) ~/ 2;
    int comparison = arr[mid].compareTo(target);
    
    if (comparison == 0) {
      return mid;
    } else if (comparison > 0) {
      high = mid - 1;
    } else {
      low = mid + 1;
    }
  }
  
  return null;
}

void main() {
  List<int> sortedList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  print('Binary Search Examples:');
  print('Searching for 5 in $sortedList: ${binarySearch(sortedList, 5)}');
  print('Searching for 11 in $sortedList: ${binarySearch(sortedList, 11)}');
  print('Iterative search for 7: ${binarySearchIterative(sortedList, 7)}');
  
  // Test with duplicates
  List<int> duplicates = [1, 2, 2, 2, 3, 4, 5];
  print('\nSearching in array with duplicates: $duplicates');
  print('Leftmost occurrence of 2: ${binarySearchLeftmost(duplicates, 2)}');
  print('Rightmost occurrence of 2: ${binarySearchRightmost(duplicates, 2)}');
  
  // Test with strings
  List<String> words = ['apple', 'banana', 'cherry', 'date', 'elderberry'];
  print('\nSearching in string array: $words');
  print('Search for "cherry": ${binarySearchGeneric(words, "cherry")}');
  print('Search for "grape": ${binarySearchGeneric(words, "grape")}');
}