/// Insertion Sort implementation
///
/// # Description
/// Insertion Sort works by building a sorted portion of the array one element at a time.
/// It is efficient for small or nearly sorted datasets.
///
/// # Time Complexity
/// - Best: O(n)
/// - Average/Worst: O(n²)
///
/// # Example
/// ```
/// let mut nums = vec![5, 2, 4, 6, 1, 3];
/// insertion_sort(&mut nums);
/// assert_eq!(nums, vec![1, 2, 3, 4, 5, 6]);
/// ```
pub fn insertion_sort(arr: &mut Vec<i32>) {
    for i in 1..arr.len() {
        let key = arr[i];
        let mut j = i;
        while j > 0 && arr[j - 1] > key {
            arr[j] = arr[j - 1];
            j -= 1;
        }
        arr[j] = key;
    }
}

