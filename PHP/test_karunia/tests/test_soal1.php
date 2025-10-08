<?php

declare(strict_types=1);

require_once __DIR__ . '/../soal1.php';

function assert_true(bool $cond, string $message): void
{
    if (!$cond) {
        throw new RuntimeException($message);
    }
}

function is_sorted(array $arr): bool
{
    for ($i = 1; $i < count($arr); $i++) {
        if ($arr[$i - 1] > $arr[$i]) {
            return false;
        }
    }
    return true;
}

// Test 1: Generate exactly 20 numbers
$arr = generate_random_integers(20, -100, 100);
assert_true(count($arr) === 20, 'Array is not of size 20');

// Test 2: Range -100..100
foreach ($arr as $v) {
    assert_true($v >= -100 && $v <= 100, 'Value out of range');
}

// Test 3: Bubble sort sorts
$sorted = bubble_sort($arr);
assert_true(is_sorted($sorted), 'Array is not non-decreasing');

// Test 4: Idempotent once sorted
$sorted2 = bubble_sort($sorted);
assert_true($sorted === $sorted2, 'Sort is not idempotent');

// Test 5: Edge case all equal
$same = array_fill(0, 20, 5);
assert_true(is_sorted(bubble_sort($same)), 'Edge all-equal failed');

// Test 6: Boundary values
$boundary = [-100, 100, -100, 100, 0];
assert_true(is_sorted(bubble_sort($boundary)), 'Boundary failed');

header('Content-Type: text/plain; charset=UTF-8');
echo "PASS: All Problem 1 tests passed\n";


