/// Quick Sort implementation
///
/// # Description
/// Quick Sort selects a pivot element and partitions the array into
/// elements less than the pivot and elements greater than the pivot.
/// It then recursively sorts the partitions.
///
/// # Time Complexity
/// - Best/Average: O(n log n)
/// - Worst: O(n²) (rare, with bad pivot selection)
///
/// # Example
/// ```
/// let mut nums = vec![3, 7, 8, 5, 2, 1, 9, 5, 4];
/// quick_sort(&mut nums);
/// assert_eq!(nums, vec![1, 2, 3, 4, 5, 5, 7, 8, 9]);
/// ```
pub fn quick_sort(arr: &mut Vec<i32>) {
    fn partition(arr: &mut Vec<i32>, low: isize, high: isize) -> isize {
        let pivot = arr[high as usize];
        let mut i = low - 1;
        for j in low..high {
            if arr[j as usize] <= pivot {
                i += 1;
                arr.swap(i as usize, j as usize);
            }
        }
        arr.swap((i + 1) as usize, high as usize);
        i + 1
    }

    fn quick_sort_recursive(arr: &mut Vec<i32>, low: isize, high: isize) {
        if low < high {
            let pi = partition(arr, low, high);
            quick_sort_recursive(arr, low, pi - 1);
            quick_sort_recursive(arr, pi + 1, high);
        }
    }

    if !arr.is_empty() {
        let len = arr.len();
        quick_sort_recursive(arr, 0, (len - 1) as isize);
    }
}

