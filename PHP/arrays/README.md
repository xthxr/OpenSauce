# Array Problems in PHP

This section contains **array-related data structure and algorithm problems**, implemented in **PHP (7+)** and organized for easy contribution and learning.
Each problem follows the [DSA PHP Template](../template.php) and adheres to **PSR-12 coding standards**.

## Folder Structure

```
arrays/
│
├── basic/                # Simple array operations
├── intermediate/         # Algorithmic and logic-based problems
├── advanced/             # Optimized and complex algorithms
├── two_pointers/         # Two-pointer & sliding window techniques
├── matrix/               # 2D array and matrix problems
├── bonus/                # Miscellaneous and real-world array problems
└── README.md
```

## Coding Guidelines

Each PHP file must:

1. Begin with:

   ```php
   <?php
   declare(strict_types=1);
   ```
2. Use **PSR-12** coding standards (4-space indentation, max 120-char lines).
3. Follow the project [template.php](../template.php) structure:

   * Problem title & description
   * Time and space complexity
   * Function with type hints
   * Example usage at the bottom
4. Use **snake_case** naming convention for filenames (e.g., `maximum_subarray_sum.php`).
5. Keep your code self-contained and well-commented.


### Basic Level Problems

| #  | Problem                    | Description                           |
| -- | -------------------------- | ------------------------------------- |
| 1  | `find_maximum_element.php` | Find the largest element in an array  |
| 2  | `find_minimum_element.php` | Find the smallest element in an array |
| 3  | `sum_of_elements.php`      | Sum of all elements                   |
| 4  | `average_of_elements.php`  | Compute average of elements           |
| 5  | `reverse_array.php`        | Reverse an array in place             |
| 6  | `check_sorted_array.php`   | Check if array is sorted              |
| 7  | `count_even_odd.php`       | Count even and odd elements           |
| 8  | `find_second_largest.php`  | Find second largest element           |
| 9  | `remove_duplicates.php`    | Remove duplicates                     |
| 10 | `rotate_array.php`         | Rotate array by *k* positions         |



### Intermediate Level Problems

| #  | Problem                       | Description                             |
| -- | ----------------------------- | --------------------------------------- |
| 1  | `pair_with_given_sum.php`     | Check if a pair exists with a given sum |
| 2  | `merge_two_sorted_arrays.php` | Merge two sorted arrays                 |
| 3  | `move_zeroes_to_end.php`      | Move all zeroes to end                  |
| 4  | `find_missing_number.php`     | Find missing number from 1…n            |
| 5  | `find_duplicate_number.php`   | Detect duplicate in array               |
| 6  | `intersection_of_arrays.php`  | Find intersection of two arrays         |
| 7  | `union_of_arrays.php`         | Find union of two arrays                |
| 8  | `max_consecutive_ones.php`    | Find max consecutive 1s                 |
| 9  | `leader_elements.php`         | Find all leader elements                |
| 10 | `equilibrium_index.php`       | Find index where left sum == right sum  |



### Advanced Level Problems

| #  | Problem                           | Description                               |
| -- | --------------------------------- | ----------------------------------------- |
| 1  | `maximum_subarray_sum.php`        | Kadane’s algorithm for max sum            |
| 2  | `maximum_product_subarray.php`    | Find contiguous subarray with max product |
| 3  | `subarray_sum_equals_k.php`       | Count subarrays summing to *k*            |
| 4  | `longest_subarray_with_sum_k.php` | Longest subarray with sum *k*             |
| 5  | `trapping_rain_water.php`         | Calculate trapped rainwater               |
| 6  | `stock_buy_sell.php`              | Max profit from one transaction           |
| 7  | `stock_buy_sell_multiple.php`     | Max profit with multiple transactions     |
| 8  | `find_majority_element.php`       | Find majority (> n/2) element             |
| 9  | `find_minimum_swaps_to_sort.php`  | Minimum swaps required to sort            |
| 10 | `rearrange_positive_negative.php` | Alternate +ve and -ve numbers             |



### Two-Pointer / Sliding Window Problems

| # | Problem                                | Description                           |
| - | -------------------------------------- | ------------------------------------- |
| 1 | `two_sum_sorted.php`                   | Find two numbers that sum to a target |
| 2 | `three_sum.php`                        | Find triplets that sum to zero        |
| 3 | `subarray_with_given_sum.php`          | Find subarray with given sum          |
| 4 | `longest_substring_without_repeat.php` | Sliding window implementation         |
| 5 | `minimum_size_subarray_sum.php`        | Smallest subarray with sum ≥ target   |
| 6 | `max_sum_subarray_of_size_k.php`       | Max sum of subarray of size *k*       |
| 7 | `find_duplicate_within_k_distance.php` | Detect duplicates within *k* distance |



### Matrix / 2D Array Problems

| # | Problem                       | Description                               |
| - | ----------------------------- | ----------------------------------------- |
| 1 | `matrix_transpose.php`        | Transpose of a matrix                     |
| 2 | `rotate_matrix_90.php`        | Rotate matrix 90° clockwise               |
| 3 | `spiral_order_traversal.php`  | Print elements in spiral order            |
| 4 | `search_in_sorted_matrix.php` | Search element efficiently in matrix      |
| 5 | `set_matrix_zeroes.php`       | Set row/column to zero if element is zero |
| 6 | `diagonal_sum.php`            | Find sum of diagonals                     |
| 7 | `saddle_point_matrix.php`     | Find saddle point                         |
| 8 | `largest_sum_submatrix.php`   | Kadane’s 2D version                       |



### Real-World Array Challenges

| #  | Problem                                         | Description                            |
| -- | ----------------------------------------------- | -------------------------------------- |
| 1  | `reorder_array_as_even_odd.php`                 | Move even numbers before odd numbers   |
| 2  | `shuffle_array.php`                             | Random shuffle (Fisher-Yates)          |
| 3  | `find_kth_largest.php`                          | Find kth largest element               |
| 4  | `merge_intervals.php`                           | Merge overlapping intervals            |
| 5  | `array_partition_equal_sum.php`                 | Partition array into equal sum subsets |
| 6  | `product_of_array_except_self.php`              | Product of all elements except self    |
| 7  | `missing_and_repeating_number.php`              | Find missing and repeating numbers     |
| 8  | `next_permutation.php`                          | Compute next permutation               |
| 9  | `smallest_subarray_with_sum_greater_than_x.php` | Sliding window approach                |
| 10 | `sort_array_by_parity.php`                      | Move evens to front, odds to end       |



### Example File

Each solution file should look like this:

```php
<?php

declare(strict_types=1);

/**
 * Problem Title: Reverse Array
 *
 * Description:
 * Reverse an array in place without using extra space.
 *
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 */

function reverseArray(array &$arr): void
{
    $left = 0;
    $right = count($arr) - 1;

    while ($left < $right) {
        [$arr[$left], $arr[$right]] = [$arr[$right], $arr[$left]];
        $left++;
        $right--;
    }
}

// Example Usage
$nums = [1, 2, 3, 4, 5];
reverseArray($nums);
print_r($nums); // [5, 4, 3, 2, 1]
```


### Goals

* Help contributors practice **DSA concepts using PHP**.
* Maintain clean, readable, and standardized code.
* Encourage open-source participation during **Hacktoberfest**.
