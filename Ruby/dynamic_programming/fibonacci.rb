# Fibonacci sequence implementations in Ruby
# Demonstrates different approaches: naive recursion, memoization, and iterative
# Time and space complexity analysis for each approach

class Fibonacci
  # Naive recursive Fibonacci (inefficient)
  # Time Complexity: O(2^n), Space Complexity: O(n)
  def self.naive(n)
    return n if n <= 1
    naive(n - 1) + naive(n - 2)
  end

  # Fibonacci using memoization
  # Time Complexity: O(n), Space Complexity: O(n)
  def self.memoized(n, memo = {})
    return n if n <= 1
    return memo[n] if memo.key?(n)
    
    memo[n] = memoized(n - 1, memo) + memoized(n - 2, memo)
  end

  # Iterative Fibonacci using variables
  # Time Complexity: O(n), Space Complexity: O(1)
  def self.iterative(n)
    return n if n <= 1
    
    prev, curr = 0, 1
    (2..n).each do
      prev, curr = curr, prev + curr
    end
    curr
  end

  # Fibonacci using dynamic programming array
  # Time Complexity: O(n), Space Complexity: O(n)
  def self.dynamic_programming(n)
    return n if n <= 1
    
    dp = Array.new(n + 1)
    dp[0], dp[1] = 0, 1
    
    (2..n).each do |i|
      dp[i] = dp[i - 1] + dp[i - 2]
    end
    dp[n]
  end

  # Matrix exponentiation approach for Fibonacci
  # Time Complexity: O(log n), Space Complexity: O(log n)
  def self.matrix_exponentiation(n)
    return n if n <= 1
    
    result = matrix_power([[1, 1], [1, 0]], n - 1)
    result[0][0]
  end

  # Matrix multiplication for 2x2 matrices
  def self.matrix_multiply(a, b)
    [
      [a[0][0] * b[0][0] + a[0][1] * b[1][0], a[0][0] * b[0][1] + a[0][1] * b[1][1]],
      [a[1][0] * b[0][0] + a[1][1] * b[1][0], a[1][0] * b[0][1] + a[1][1] * b[1][1]]
    ]
  end

  # Matrix exponentiation
  def self.matrix_power(matrix, n)
    return [[1, 0], [0, 1]] if n == 0  # Identity matrix
    return matrix if n == 1
    
    if n.even?
      half = matrix_power(matrix, n / 2)
      matrix_multiply(half, half)
    else
      matrix_multiply(matrix, matrix_power(matrix, n - 1))
    end
  end

  # Golden ratio approach (Binet's formula)
  # Time Complexity: O(1), Space Complexity: O(1)
  # Note: Limited by floating point precision for large n
  def self.golden_ratio(n)
    phi = (1 + Math.sqrt(5)) / 2
    psi = (1 - Math.sqrt(5)) / 2
    ((phi**n - psi**n) / Math.sqrt(5)).round
  end

  # Generate first n Fibonacci numbers
  # Time Complexity: O(n), Space Complexity: O(n)
  def self.sequence(n)
    return [] if n <= 0
    return [0] if n == 1
    
    sequence = [0, 1]
    (2...n).each do |i|
      sequence << sequence[i - 1] + sequence[i - 2]
    end
    sequence
  end

  # Check if a number is a Fibonacci number
  # Time Complexity: O(log n), Space Complexity: O(1)
  def self.is_fibonacci?(n)
    perfect_square?(5 * n * n + 4) || perfect_square?(5 * n * n - 4)
  end

  # Sum of first n Fibonacci numbers
  # Time Complexity: O(n), Space Complexity: O(1)
  def self.sum_of_first_n(n)
    iterative(n + 2) - 1
  end

  private

  def self.perfect_square?(x)
    return false if x < 0
    root = Math.sqrt(x).to_i
    root * root == x
  end
end

# Example usage and benchmarking
if __FILE__ == $0
  puts "Fibonacci Sequence Implementations:"
  
  n = 20
  puts "\nFirst #{n} Fibonacci numbers:"
  puts Fibonacci.sequence(n).to_s
  
  puts "\nFibonacci(#{n}) using different methods:"
  puts "Naive recursion: #{Fibonacci.naive(n)}"
  puts "Memoized: #{Fibonacci.memoized(n)}"
  puts "Iterative: #{Fibonacci.iterative(n)}"
  puts "Dynamic Programming: #{Fibonacci.dynamic_programming(n)}"
  puts "Matrix exponentiation: #{Fibonacci.matrix_exponentiation(n)}"
  puts "Golden ratio: #{Fibonacci.golden_ratio(n)}"
  
  puts "\nSum of first #{n} Fibonacci numbers: #{Fibonacci.sum_of_first_n(n)}"
  
  puts "\nChecking if numbers are Fibonacci:"
  test_numbers = [0, 1, 2, 3, 5, 8, 13, 21, 34, 35, 55]
  test_numbers.each do |num|
    puts "#{num} is Fibonacci: #{Fibonacci.is_fibonacci?(num)}"
  end
  
  puts "\nPerformance note:"
  puts "- Naive recursion: Very slow for large n (exponential time)"
  puts "- Memoized: Good balance of elegance and performance"
  puts "- Iterative: Most memory efficient"
  puts "- Matrix exponentiation: Best for very large n (logarithmic time)"
end