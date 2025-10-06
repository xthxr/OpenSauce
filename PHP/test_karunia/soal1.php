<?php

declare(strict_types=1);

/**
 * Problem 1
 * Generate 20 random integers in the interval [-100, 100], then sort them
 * without using any built-in sort function.
 *
 * Algorithm: Bubble Sort
 * - Time: O(n^2)
 * - Space: O(1)
 */

/**
 * Generate an array of random integers of size $count within [$min, $max].
 *
 * @return int[]
 */
function generate_random_integers(int $count, int $min, int $max): array
{
    $result = [];
    for ($i = 0; $i < $count; $i++) {
        $result[] = random_int($min, $max);
    }
    return $result;
}

/**
 * Sort an integer array using Bubble Sort and return the result.
 *
 * @param int[] $numbers
 * @return int[]
 */
function bubble_sort(array $numbers): array
{
    $n = count($numbers);
    if ($n < 2) {
        return $numbers;
    }

    for ($i = 0; $i < $n - 1; $i++) {
        $swapped = false;
        for ($j = 0; $j < $n - $i - 1; $j++) {
            if ($numbers[$j] > $numbers[$j + 1]) {
                $temp = $numbers[$j];
                $numbers[$j] = $numbers[$j + 1];
                $numbers[$j + 1] = $temp;
                $swapped = true;
            }
        }
        if (!$swapped) {
            break; // already sorted
        }
    }
    return $numbers;
}

// Generate and sort
$original = generate_random_integers(20, -100, 100);
$sorted = bubble_sort($original);

// Display output in the requested format
function format_series(array $nums): string
{
    return implode(', ', array_map(static fn($n) => (string)$n, $nums));
}

header('Content-Type: text/plain; charset=UTF-8');
echo 'Initial Series : ' . format_series($original) . "\n";
echo 'Sorted Series : ' . format_series($sorted) . "\n";


