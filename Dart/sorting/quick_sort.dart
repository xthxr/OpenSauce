/// Quick sort implementation in Dart
/// Time Complexity: Average O(n log n), Worst O(n²)
/// Space Complexity: O(log n) average, O(n) worst case

/// Quick sort implementation (functional approach)
List<T> quickSort<T extends Comparable<T>>(List<T> arr) {
  if (arr.length <= 1) return List<T>.from(arr);
  
  T pivot = arr[arr.length ~/ 2];
  List<T> smaller = arr.where((x) => x.compareTo(pivot) < 0).toList();
  List<T> equal = arr.where((x) => x.compareTo(pivot) == 0).toList();
  List<T> larger = arr.where((x) => x.compareTo(pivot) > 0).toList();
  
  return [...quickSort(smaller), ...equal, ...quickSort(larger)];
}

/// In-place quick sort implementation
void quickSortInPlace(List<int> arr, [int? low, int? high]) {
  low ??= 0;
  high ??= arr.length - 1;
  
  if (low < high) {
    int pivotIndex = _partition(arr, low, high);
    quickSortInPlace(arr, low, pivotIndex - 1);
    quickSortInPlace(arr, pivotIndex + 1, high);
  }
}

/// Partition function for in-place quick sort
int _partition(List<int> arr, int low, int high) {
  int pivot = arr[high]; // Choose last element as pivot
  int i = low - 1; // Index of smaller element
  
  for (int j = low; j < high; j++) {
    if (arr[j] <= pivot) {
      i++;
      _swap(arr, i, j);
    }
  }
  
  _swap(arr, i + 1, high);
  return i + 1;
}

/// Helper function to swap elements
void _swap(List<int> arr, int i, int j) {
  int temp = arr[i];
  arr[i] = arr[j];
  arr[j] = temp;
}

/// Quick sort with random pivot selection
List<T> quickSortRandomPivot<T extends Comparable<T>>(List<T> arr) {
  if (arr.length <= 1) return List<T>.from(arr);
  
  // Select random pivot
  int randomIndex = (arr.length * (DateTime.now().millisecondsSinceEpoch % 1000) / 1000).floor();
  T pivot = arr[randomIndex];
  
  List<T> smaller = arr.where((x) => x.compareTo(pivot) < 0).toList();
  List<T> equal = arr.where((x) => x.compareTo(pivot) == 0).toList();
  List<T> larger = arr.where((x) => x.compareTo(pivot) > 0).toList();
  
  return [...quickSortRandomPivot(smaller), ...equal, ...quickSortRandomPivot(larger)];
}

/// Quick sort with custom comparator
List<T> quickSortWithComparator<T>(List<T> arr, int Function(T, T) compare) {
  if (arr.length <= 1) return List<T>.from(arr);
  
  T pivot = arr[arr.length ~/ 2];
  List<T> smaller = arr.where((x) => compare(x, pivot) < 0).toList();
  List<T> equal = arr.where((x) => compare(x, pivot) == 0).toList();
  List<T> larger = arr.where((x) => compare(x, pivot) > 0).toList();
  
  return [
    ...quickSortWithComparator(smaller, compare),
    ...equal,
    ...quickSortWithComparator(larger, compare)
  ];
}

/// Three-way partitioning quick sort (Dutch National Flag)
/// Efficient for arrays with many duplicate elements
List<int> quickSort3Way(List<int> arr) {
  if (arr.length <= 1) return List<int>.from(arr);
  
  List<int> result = List<int>.from(arr);
  _quickSort3WayHelper(result, 0, result.length - 1);
  return result;
}

void _quickSort3WayHelper(List<int> arr, int low, int high) {
  if (low >= high) return;
  
  List<int> partitionResult = _partition3Way(arr, low, high);
  int lt = partitionResult[0]; // Elements less than pivot end here
  int gt = partitionResult[1]; // Elements greater than pivot start here
  
  _quickSort3WayHelper(arr, low, lt - 1);
  _quickSort3WayHelper(arr, gt + 1, high);
}

/// Three-way partition: returns [lt, gt] where
/// arr[low..lt-1] < pivot, arr[lt..gt] == pivot, arr[gt+1..high] > pivot
List<int> _partition3Way(List<int> arr, int low, int high) {
  int pivot = arr[low];
  int i = low;
  int lt = low;
  int gt = high;
  
  while (i <= gt) {
    if (arr[i] < pivot) {
      _swap(arr, lt++, i++);
    } else if (arr[i] > pivot) {
      _swap(arr, i, gt--);
    } else {
      i++;
    }
  }
  
  return [lt, gt];
}

/// Iterative quick sort implementation
List<int> quickSortIterative(List<int> arr) {
  if (arr.length <= 1) return List<int>.from(arr);
  
  List<int> result = List<int>.from(arr);
  List<int> stack = [];
  
  // Push initial bounds
  stack.add(0);
  stack.add(result.length - 1);
  
  while (stack.isNotEmpty) {
    int high = stack.removeLast();
    int low = stack.removeLast();
    
    if (low < high) {
      int pivotIndex = _partition(result, low, high);
      
      // Push left subarray bounds
      stack.add(low);
      stack.add(pivotIndex - 1);
      
      // Push right subarray bounds
      stack.add(pivotIndex + 1);
      stack.add(high);
    }
  }
  
  return result;
}

void main() {
  List<int> unsortedList = [64, 34, 25, 12, 22, 11, 90];
  print('Quick Sort Examples:');
  print('Original: $unsortedList');
  print('Sorted (functional): ${quickSort(unsortedList)}');
  
  // Test in-place quick sort
  List<int> inPlaceList = List<int>.from(unsortedList);
  quickSortInPlace(inPlaceList);
  print('Sorted (in-place): $inPlaceList');
  
  print('Sorted (random pivot): ${quickSortRandomPivot(unsortedList)}');
  print('Sorted (3-way): ${quickSort3Way(unsortedList)}');
  print('Sorted (iterative): ${quickSortIterative(unsortedList)}');
  
  // Test with strings
  String stringToSort = 'quicksort';
  List<String> chars = stringToSort.split('');
  print('\nSorting string "$stringToSort": ${quickSort(chars).join('')}');
  
  // Test with custom comparator (descending order)
  List<int> descendingSort = quickSortWithComparator(unsortedList, (a, b) => b.compareTo(a));
  print('Descending sort: $descendingSort');
  
  // Test with duplicates
  List<int> withDuplicates = [3, 6, 8, 10, 1, 2, 1, 5, 3, 3];
  print('\nWith duplicates: $withDuplicates');
  print('3-way sort result: ${quickSort3Way(withDuplicates)}');
}