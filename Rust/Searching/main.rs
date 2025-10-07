mod searching;   // Import searching.rs
use crate::searching::*;

fn main() {
    let arr = [1, 3, 5, 7, 9];

    println!("Linear search 5: {:?}", linear_search(&arr, 5));
    println!("Binary search 7: {:?}", binary_search(&arr, 7));
}
