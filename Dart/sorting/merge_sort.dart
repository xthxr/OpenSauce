/// Merge sort implementation in Dart
/// Time Complexity: O(n log n)
/// Space Complexity: O(n)

/// Merge sort implementation
List<T> mergeSort<T extends Comparable<T>>(List<T> arr) {
  if (arr.length <= 1) return List<T>.from(arr);
  
  int mid = arr.length ~/ 2;
  List<T> left = arr.sublist(0, mid);
  List<T> right = arr.sublist(mid);
  
  return merge(mergeSort(left), mergeSort(right));
}

/// Merge two sorted lists into one sorted list
List<T> merge<T extends Comparable<T>>(List<T> left, List<T> right) {
  List<T> result = [];
  int i = 0, j = 0;
  
  while (i < left.length && j < right.length) {
    if (left[i].compareTo(right[j]) <= 0) {
      result.add(left[i]);
      i++;
    } else {
      result.add(right[j]);
      j++;
    }
  }
  
  // Add remaining elements
  while (i < left.length) {
    result.add(left[i]);
    i++;
  }
  
  while (j < right.length) {
    result.add(right[j]);
    j++;
  }
  
  return result;
}

/// In-place merge sort implementation (modifies original array)
void mergeSortInPlace(List<int> arr, [int? left, int? right]) {
  left ??= 0;
  right ??= arr.length - 1;
  
  if (left < right) {
    int mid = left + (right - left) ~/ 2;
    
    mergeSortInPlace(arr, left, mid);
    mergeSortInPlace(arr, mid + 1, right);
    
    _mergeInPlace(arr, left, mid, right);
  }
}

/// Helper function for in-place merge
void _mergeInPlace(List<int> arr, int left, int mid, int right) {
  // Create temporary arrays
  List<int> leftArr = arr.sublist(left, mid + 1);
  List<int> rightArr = arr.sublist(mid + 1, right + 1);
  
  int i = 0, j = 0, k = left;
  
  // Merge the temporary arrays back into arr[left..right]
  while (i < leftArr.length && j < rightArr.length) {
    if (leftArr[i] <= rightArr[j]) {
      arr[k] = leftArr[i];
      i++;
    } else {
      arr[k] = rightArr[j];
      j++;
    }
    k++;
  }
  
  // Copy remaining elements
  while (i < leftArr.length) {
    arr[k] = leftArr[i];
    i++;
    k++;
  }
  
  while (j < rightArr.length) {
    arr[k] = rightArr[j];
    j++;
    k++;
  }
}

/// Bottom-up merge sort implementation
List<T> mergeSortBottomUp<T extends Comparable<T>>(List<T> arr) {
  if (arr.length <= 1) return List<T>.from(arr);
  
  List<T> result = List<T>.from(arr);
  int n = result.length;
  
  // Merge subarrays in bottom-up manner
  for (int size = 1; size < n; size *= 2) {
    for (int left = 0; left < n - size; left += 2 * size) {
      int mid = left + size - 1;
      int right = (left + 2 * size - 1).clamp(0, n - 1);
      
      if (mid < right) {
        List<T> leftPart = result.sublist(left, mid + 1);
        List<T> rightPart = result.sublist(mid + 1, right + 1);
        List<T> merged = merge(leftPart, rightPart);
        
        for (int i = 0; i < merged.length; i++) {
          result[left + i] = merged[i];
        }
      }
    }
  }
  
  return result;
}

/// Merge sort with custom comparator
List<T> mergeSortWithComparator<T>(List<T> arr, int Function(T, T) compare) {
  if (arr.length <= 1) return List<T>.from(arr);
  
  int mid = arr.length ~/ 2;
  List<T> left = arr.sublist(0, mid);
  List<T> right = arr.sublist(mid);
  
  return _mergeWithComparator(
    mergeSortWithComparator(left, compare),
    mergeSortWithComparator(right, compare),
    compare
  );
}

List<T> _mergeWithComparator<T>(List<T> left, List<T> right, int Function(T, T) compare) {
  List<T> result = [];
  int i = 0, j = 0;
  
  while (i < left.length && j < right.length) {
    if (compare(left[i], right[j]) <= 0) {
      result.add(left[i]);
      i++;
    } else {
      result.add(right[j]);
      j++;
    }
  }
  
  result.addAll(left.sublist(i));
  result.addAll(right.sublist(j));
  
  return result;
}

void main() {
  List<int> unsortedList = [38, 27, 43, 3, 9, 82, 10];
  print('Merge Sort Examples:');
  print('Original: $unsortedList');
  print('Sorted (functional): ${mergeSort(unsortedList)}');
  
  // Test in-place merge sort
  List<int> inPlaceList = List<int>.from(unsortedList);
  mergeSortInPlace(inPlaceList);
  print('Sorted (in-place): $inPlaceList');
  
  print('Sorted (bottom-up): ${mergeSortBottomUp(unsortedList)}');
  
  // Test with strings
  List<String> words = ['merge', 'sort', 'algorithm', 'divide', 'conquer'];
  print('\nSorting words: $words');
  print('Result: ${mergeSort(words)}');
  
  // Test with custom comparator (descending order)
  List<int> descendingSort = mergeSortWithComparator(unsortedList, (a, b) => b.compareTo(a));
  print('\nDescending sort: $descendingSort');
}