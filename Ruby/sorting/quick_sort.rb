# Quick sort implementation in Ruby
# Time Complexity: Average O(n log n), Worst O(n²)
# Space Complexity: O(log n) average, O(n) worst case

class QuickSort
  # Standard quicksort implementation
  def self.sort(arr)
    return arr if arr.length <= 1
    
    pivot = arr[0]
    smaller = arr[1..-1].select { |x| x <= pivot }
    larger = arr[1..-1].select { |x| x > pivot }
    
    sort(smaller) + [pivot] + sort(larger)
  end

  # Quicksort with different pivot selection strategies
  def self.sort_with_pivot_strategy(arr, strategy = :first)
    return arr if arr.length <= 1
    
    pivot_index = case strategy
                  when :first then 0
                  when :last then arr.length - 1
                  when :middle then arr.length / 2
                  when :random then rand(arr.length)
                  else 0
                  end
    
    pivot = arr[pivot_index]
    rest = arr[0...pivot_index] + arr[pivot_index+1..-1]
    
    smaller = rest.select { |x| x <= pivot }
    larger = rest.select { |x| x > pivot }
    
    sort_with_pivot_strategy(smaller, strategy) + [pivot] + sort_with_pivot_strategy(larger, strategy)
  end

  # In-place quicksort (more memory efficient)
  def self.sort_in_place(arr, low = 0, high = nil)
    high = arr.length - 1 if high.nil?
    
    if low < high
      # Partition the array and get pivot index
      pivot_index = partition(arr, low, high)
      
      # Recursively sort elements before and after partition
      sort_in_place(arr, low, pivot_index - 1)
      sort_in_place(arr, pivot_index + 1, high)
    end
    
    arr
  end

  # Partition helper for in-place quicksort (Lomuto partition scheme)
  def self.partition(arr, low, high)
    # Choose the rightmost element as pivot
    pivot = arr[high]
    
    # Index of smaller element (indicates right position of pivot)
    i = low - 1
    
    (low...high).each do |j|
      # If current element is smaller than or equal to pivot
      if arr[j] <= pivot
        i += 1
        arr[i], arr[j] = arr[j], arr[i]
      end
    end
    
    # Place pivot at the correct position
    arr[i + 1], arr[high] = arr[high], arr[i + 1]
    i + 1
  end

  # Hoare partition scheme (alternative partitioning)
  def self.hoare_partition(arr, low, high)
    pivot = arr[low]
    i = low - 1
    j = high + 1
    
    loop do
      # Find element on left that should be on right
      loop do
        i += 1
        break if arr[i] >= pivot
      end
      
      # Find element on right that should be on left
      loop do
        j -= 1
        break if arr[j] <= pivot
      end
      
      # If elements crossed, partitioning is done
      return j if i >= j
      
      # Swap elements
      arr[i], arr[j] = arr[j], arr[i]
    end
  end

  # Quicksort with Hoare partitioning
  def self.sort_hoare(arr, low = 0, high = nil)
    high = arr.length - 1 if high.nil?
    
    if low < high
      pivot_index = hoare_partition(arr, low, high)
      sort_hoare(arr, low, pivot_index)
      sort_hoare(arr, pivot_index + 1, high)
    end
    
    arr
  end

  # Hybrid approach: use insertion sort for small arrays
  def self.sort_hybrid(arr, low = 0, high = nil, threshold = 10)
    high = arr.length - 1 if high.nil?
    
    if high - low + 1 <= threshold
      insertion_sort(arr, low, high)
    elsif low < high
      pivot_index = partition(arr, low, high)
      sort_hybrid(arr, low, pivot_index - 1, threshold)
      sort_hybrid(arr, pivot_index + 1, high, threshold)
    end
    
    arr
  end

  # Insertion sort for small arrays
  def self.insertion_sort(arr, low, high)
    (low + 1..high).each do |i|
      key = arr[i]
      j = i - 1
      
      while j >= low && arr[j] > key
        arr[j + 1] = arr[j]
        j -= 1
      end
      
      arr[j + 1] = key
    end
  end
end

# Example usage and testing
if __FILE__ == $0
  puts "Quick Sort Examples:"
  
  unsorted_list = [64, 34, 25, 12, 22, 11, 90]
  puts "Original: #{unsorted_list}"
  puts "Sorted (standard): #{QuickSort.sort(unsorted_list.dup)}"
  puts "Sorted (in-place): #{QuickSort.sort_in_place(unsorted_list.dup)}"
  puts "Sorted (Hoare partition): #{QuickSort.sort_hoare(unsorted_list.dup)}"
  puts "Sorted (hybrid): #{QuickSort.sort_hybrid(unsorted_list.dup)}"
  
  string_to_sort = "quicksort".chars
  puts "\nSorting characters '#{string_to_sort.join}': #{QuickSort.sort(string_to_sort).join}"
  
  # Test different pivot strategies
  puts "\nTesting different pivot strategies:"
  test_array = [3, 6, 8, 10, 1, 2, 1]
  [:first, :last, :middle, :random].each do |strategy|
    result = QuickSort.sort_with_pivot_strategy(test_array.dup, strategy)
    puts "#{strategy.to_s.capitalize} pivot: #{result}"
  end
  
  # Test edge cases
  puts "\nEdge cases:"
  puts "Empty array: #{QuickSort.sort([])}"
  puts "Single element: #{QuickSort.sort([42])}"
  puts "Already sorted: #{QuickSort.sort([1, 2, 3, 4, 5])}"
  puts "Reverse sorted: #{QuickSort.sort([5, 4, 3, 2, 1])}"
  puts "All same elements: #{QuickSort.sort([3, 3, 3, 3, 3])}"
  
  # Performance characteristics
  puts "\nQuick Sort Characteristics:"
  puts "- Average Time Complexity: O(n log n)"
  puts "- Worst Time Complexity: O(n²) - when pivot is always min/max"
  puts "- Space Complexity: O(log n) average, O(n) worst case"
  puts "- Not stable: May change relative order of equal elements"
  puts "- In-place: Can sort with minimal extra memory"
  puts "- Cache-friendly: Good locality of reference"
end