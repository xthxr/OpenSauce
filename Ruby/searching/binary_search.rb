# Binary search implementation in Ruby
# Time Complexity: O(log n)
# Space Complexity: O(log n) for recursive, O(1) for iterative

class BinarySearch
  # Recursive binary search implementation
  # Time Complexity: O(log n), Space Complexity: O(log n)
  def self.recursive(arr, target, low = 0, high = nil)
    high = arr.length - 1 if high.nil?
    return nil if low > high
    
    mid = (low + high) / 2
    
    case arr[mid] <=> target
    when 0
      mid
    when 1
      recursive(arr, target, low, mid - 1)
    when -1
      recursive(arr, target, mid + 1, high)
    end
  end

  # Iterative binary search implementation
  # Time Complexity: O(log n), Space Complexity: O(1)
  def self.iterative(arr, target)
    low = 0
    high = arr.length - 1
    
    while low <= high
      mid = (low + high) / 2
      
      case arr[mid] <=> target
      when 0
        return mid
      when 1
        high = mid - 1
      when -1
        low = mid + 1
      end
    end
    
    nil
  end

  # Binary search that returns the leftmost occurrence
  # Time Complexity: O(log n), Space Complexity: O(1)
  def self.find_first(arr, target)
    low = 0
    high = arr.length - 1
    result = nil
    
    while low <= high
      mid = (low + high) / 2
      
      if arr[mid] == target
        result = mid
        high = mid - 1  # Continue searching left
      elsif arr[mid] < target
        low = mid + 1
      else
        high = mid - 1
      end
    end
    
    result
  end

  # Binary search that returns the rightmost occurrence
  # Time Complexity: O(log n), Space Complexity: O(1)
  def self.find_last(arr, target)
    low = 0
    high = arr.length - 1
    result = nil
    
    while low <= high
      mid = (low + high) / 2
      
      if arr[mid] == target
        result = mid
        low = mid + 1  # Continue searching right
      elsif arr[mid] < target
        low = mid + 1
      else
        high = mid - 1
      end
    end
    
    result
  end

  # Find insertion point for target in sorted array
  # Time Complexity: O(log n), Space Complexity: O(1)
  def self.insertion_point(arr, target)
    low = 0
    high = arr.length
    
    while low < high
      mid = (low + high) / 2
      
      if arr[mid] < target
        low = mid + 1
      else
        high = mid
      end
    end
    
    low
  end
end

# Example usage and testing
if __FILE__ == $0
  puts "Binary Search Examples:"
  
  sorted_list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  puts "Sorted array: #{sorted_list}"
  
  # Test basic searches
  target = 5
  puts "\nSearching for #{target}:"
  puts "Recursive: #{BinarySearch.recursive(sorted_list, target)}"
  puts "Iterative: #{BinarySearch.iterative(sorted_list, target)}"
  
  # Test not found
  target = 11
  puts "\nSearching for #{target} (not in array):"
  puts "Recursive: #{BinarySearch.recursive(sorted_list, target)}"
  puts "Iterative: #{BinarySearch.iterative(sorted_list, target)}"
  
  # Test with duplicates
  arr_with_duplicates = [1, 2, 2, 2, 3, 4, 5, 5, 5, 6]
  target = 2
  puts "\nArray with duplicates: #{arr_with_duplicates}"
  puts "Searching for #{target}:"
  puts "Any occurrence: #{BinarySearch.iterative(arr_with_duplicates, target)}"
  puts "First occurrence: #{BinarySearch.find_first(arr_with_duplicates, target)}"
  puts "Last occurrence: #{BinarySearch.find_last(arr_with_duplicates, target)}"
  
  target = 5
  puts "\nSearching for #{target}:"
  puts "First occurrence: #{BinarySearch.find_first(arr_with_duplicates, target)}"
  puts "Last occurrence: #{BinarySearch.find_last(arr_with_duplicates, target)}"
  
  # Test insertion point
  puts "\nInsertion points:"
  [0, 2.5, 5, 7].each do |val|
    puts "Insertion point for #{val}: #{BinarySearch.insertion_point(sorted_list, val)}"
  end
end