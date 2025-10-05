/// Bubble Sort implementation
/// 
/// # Arguments
/// * `arr` - A mutable reference to a vector of integers
///
/// # Example
/// ```
/// let mut nums = vec![5, 2, 9, 1];
/// bubble_sort(&mut nums);
/// assert_eq!(nums, vec![1, 2, 5, 9]);
/// ```
pub fn bubble_sort(arr: &mut Vec<i32>) {
    let n = arr.len();
    for i in 0..n {
        for j in 0..n - i - 1 {
            if arr[j] > arr[j + 1] {
                arr.swap(j, j + 1);
            }
        }
    }
}

