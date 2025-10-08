/// Fibonacci sequence implementations in Dart
/// Demonstrates different approaches: naive recursion, memoization, and iterative
/// Time and space complexity analysis for each approach

import 'dart:math';

/// Naive recursive Fibonacci (inefficient)
/// Time Complexity: O(2^n), Space Complexity: O(n)
int fibonacciNaive(int n) {
  if (n <= 1) return n;
  return fibonacciNaive(n - 1) + fibonacciNaive(n - 2);
}

/// Fibonacci using memoization with Map
/// Time Complexity: O(n), Space Complexity: O(n)
Map<int, int> _fibMemo = {};

int fibonacciMemoized(int n) {
  if (n <= 1) return n;
  if (_fibMemo.containsKey(n)) return _fibMemo[n]!;
  
  int result = fibonacciMemoized(n - 1) + fibonacciMemoized(n - 2);
  _fibMemo[n] = result;
  return result;
}

/// Iterative Fibonacci using accumulator
/// Time Complexity: O(n), Space Complexity: O(1)
int fibonacciIterative(int n) {
  if (n <= 1) return n;
  
  int a = 0, b = 1;
  for (int i = 2; i <= n; i++) {
    int temp = a + b;
    a = b;
    b = temp;
  }
  return b;
}

/// Matrix exponentiation approach for Fibonacci
/// Time Complexity: O(log n), Space Complexity: O(log n)
int fibonacciMatrix(int n) {
  if (n <= 1) return n;
  
  List<List<int>> result = matrixPower([[1, 1], [1, 0]], n - 1);
  return result[0][0];
}

/// Matrix multiplication for 2x2 matrices
List<List<int>> matrixMultiply(List<List<int>> a, List<List<int>> b) {
  return [
    [a[0][0] * b[0][0] + a[0][1] * b[1][0], a[0][0] * b[0][1] + a[0][1] * b[1][1]],
    [a[1][0] * b[0][0] + a[1][1] * b[1][0], a[1][0] * b[0][1] + a[1][1] * b[1][1]]
  ];
}

/// Matrix exponentiation
List<List<int>> matrixPower(List<List<int>> matrix, int n) {
  if (n == 0) return [[1, 0], [0, 1]]; // Identity matrix
  if (n == 1) return matrix;
  
  if (n % 2 == 0) {
    List<List<int>> half = matrixPower(matrix, n ~/ 2);
    return matrixMultiply(half, half);
  } else {
    return matrixMultiply(matrix, matrixPower(matrix, n - 1));
  }
}

/// Golden ratio approach (Binet's formula)
/// Time Complexity: O(1), Space Complexity: O(1)
/// Note: Limited by floating point precision for large n
int fibonacciBinet(int n) {
  double phi = (1 + sqrt(5)) / 2; // Golden ratio
  double psi = (1 - sqrt(5)) / 2; // Conjugate of golden ratio
  double sqrt5 = sqrt(5);
  
  return ((pow(phi, n) - pow(psi, n)) / sqrt5).round();
}

/// Generate first n Fibonacci numbers
/// Time Complexity: O(n), Space Complexity: O(n)
List<int> fibonacciSequence(int n) {
  if (n <= 0) return [];
  if (n == 1) return [0];
  
  List<int> sequence = [0, 1];
  for (int i = 2; i < n; i++) {
    sequence.add(sequence[i - 1] + sequence[i - 2]);
  }
  return sequence;
}

/// Check if a number is a Fibonacci number
/// Time Complexity: O(log n), Space Complexity: O(1)
bool isFibonacci(int n) {
  return _isPerfectSquare(5 * n * n + 4) || _isPerfectSquare(5 * n * n - 4);
}

bool _isPerfectSquare(int x) {
  int root = sqrt(x).floor();
  return root * root == x;
}

/// Sum of first n Fibonacci numbers
/// Time Complexity: O(n), Space Complexity: O(1)
int fibonacciSum(int n) {
  return fibonacciIterative(n + 2) - 1;
}

void main() {
  print('Fibonacci Sequence Implementations:');
  
  int n = 20;
  print('\nFirst $n Fibonacci numbers:');
  print(fibonacciSequence(n));
  
  print('\nFibonacci($n) using different methods:');
  print('Naive recursion (n=10): ${fibonacciNaive(10)}'); // Using smaller n for performance
  print('Memoized: ${fibonacciMemoized(n)}');
  print('Iterative: ${fibonacciIterative(n)}');
  print('Matrix exponentiation: ${fibonacciMatrix(n)}');
  print('Binet\'s formula: ${fibonacciBinet(n)}');
  
  print('\nSum of first $n Fibonacci numbers: ${fibonacciSum(n)}');
  print('Is 21 a Fibonacci number? ${isFibonacci(21)}');
  print('Is 22 a Fibonacci number? ${isFibonacci(22)}');
}