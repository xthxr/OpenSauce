<?php

declare(strict_types=1);

/**
 * Problem: Find Maximum Element in an Array
 * Category: Arrays
 * Difficulty: Easy
 * Language: PHP
 * 
 * Approach:
 *  - Initialize max with the first element.
 *  - Traverse the array.
 *  - Update max whenever a larger element is found.
 *  - Return the max value.
 */
function findMaximumElement(array $arr): int {
    if (empty($arr)) {
        throw new InvalidArgumentException("Array must not be empty.");
    }

    $maxElement = $arr[0];

    foreach ($arr as $value) {
        if ($value > $maxElement) {
            $maxElement = $value;
        }
    }

    return $maxElement;
}

// Example usage
$numbers = [3, 5, 7, 2, 8];
echo "Array elements: [" . implode(", ", $numbers) . "]" . PHP_EOL; // Output: Array elements
echo "Maximum element: " . findMaximumElement($numbers) . PHP_EOL; // Output: 8
