// searching.rs
pub fn linear_search(arr: &[i32], target: i32) -> Option<usize> {
    for (i, &val) in arr.iter().enumerate() {
        if val == target { return Some(i); }
    }
    None
}

pub fn binary_search(arr: &[i32], target: i32) -> Option<usize> {
    let mut left = 0;
    let mut right = arr.len().saturating_sub(1);

    while left <= right {
        let mid = left + (right - left) / 2;
        if arr[mid] == target { return Some(mid); }
        else if arr[mid] < target { left = mid + 1; }
        else { right = right.saturating_sub(1); }
    }
    None
}
