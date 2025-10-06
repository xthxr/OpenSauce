# Test Karunia - PHP Programming

This repository contains solutions for 2 PHP programming problems with a total of 110 points.

## How to Run

Prerequisites:

- PHP 7.4 or newer
- Web browser

Steps:

1. Open a terminal in `PHP/test_karunia`
2. Start the server: `php -S localhost:8000`
3. Open in your browser:
   - Problem 1: `http://localhost:8000/soal1.php`
   - Problem 2: `http://localhost:8000/soal2.php`
   - Test Runner: `http://localhost:8000/tests/run_all_tests.php`

## Structure

```
test_karunia/
├── soal1.php          # Problem 1 Solution - Array Sorting (Bubble Sort)
├── soal2.php          # Problem 2 Solution - Hollow Diamond (Manhattan distance)
└── tests/
    ├── run_all_tests.php  # Test Runner
    ├── test_soal1.php     # Test Suite for Problem 1
    └── test_soal2.php     # Test Suite for Problem 2
```

## Notes

- Code follows PSR-12 where possible and uses type hints.
- Each refresh produces random numbers/size as required.
- Run the test suite to ensure all requirements are met.

## Problems and Scoring

### Problem 1 (40 points)

Generate a sequence of 20 random integers (range -100 to 100), then sort the sequence without using PHP built-in sorting functions.

Scoring breakdown:

- 10 points: Can display 20 numbers
- 10 points: Numbers change on each page refresh
- 20 points: Sequence is correctly sorted as required

Example output:

```
Initial Series : 44, -14, -71, 67, 20, 55, -20, 73, 34, 37, -19, -81, 35, 55, -86, 89, -22, -19, -24, 14
Sorted Series : -86, -81, -71, -24, -22, -20, -19, -19, -14, 14, 20, 34, 35, 37, 44, 55, 55, 67, 73, 89
```

### Problem 2 (70 points)

Render a hollow diamond shape as shown in the sample. The size must be random on each page refresh and the diamond must be symmetric.

Scoring breakdown:

- 30 points: Can display a diamond shape
- 10 points: Diamond size is random on each refresh
- 10 points: Diamond is symmetric (does not need to be ASCII boxes)
- 20 points: Output matches the problem’s sample shape

Maximum score: 110 points

## Technology

- PHP for logic
- HTML for structure (Problem 2)
- CSS for styling (Problem 2)
- Bubble Sort algorithm (for sorting without built-in functions)

## Key Features

### Problem 1 - Array Sorting

- Generates 20 random integers (-100 to 100)
- Manual Bubble Sort implementation
- Output format matches the example
- Numbers change on each refresh

### Problem 2 - Hollow Diamond

- Symmetric diamond using Manhattan distance
- Random size (radius 6–14) on each refresh
- Visual display with CSS
- Responsive grid

## Algorithms Used

### Problem 1 - Bubble Sort

```php
for ($i = 0; $i < $n - 1; $i++) {
    for ($j = 0; $j < $n - $i - 1; $j++) {
        if ($numbers[$j] > $numbers[$j + 1]) {
            $temp = $numbers[$j];
            $numbers[$j] = $numbers[$j + 1];
            $numbers[$j + 1] = $temp;
        }
    }
}
```

### Problem 2 - Manhattan Distance (edge condition)

```php
$isEdge = (abs($r - $center) + abs($c - $center) == $radius);
```

## Testing

Open the test runner in your browser:

- Test Runner: `http://localhost:8000/tests/run_all_tests.php`
- Problem 1 tests: `http://localhost:8000/tests/test_soal1.php`
- Problem 2 tests: `http://localhost:8000/tests/test_soal2.php`

All tests should pass to achieve full points.
