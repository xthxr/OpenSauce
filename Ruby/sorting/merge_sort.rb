# Merge sort implementation in Ruby
# Time Complexity: O(n log n)
# Space Complexity: O(n)

class MergeSort
  # Standard merge sort implementation
  def self.sort(arr)
    return arr if arr.length <= 1
    
    mid = arr.length / 2
    left = sort(arr[0...mid])
    right = sort(arr[mid..-1])
    
    merge(left, right)
  end

  # Merge two sorted arrays into one sorted array
  def self.merge(left, right)
    result = []
    i = j = 0
    
    # Merge elements while both arrays have elements
    while i < left.length && j < right.length
      if left[i] <= right[j]
        result << left[i]
        i += 1
      else
        result << right[j]
        j += 1
      end
    end
    
    # Add remaining elements
    result.concat(left[i..-1]) if i < left.length
    result.concat(right[j..-1]) if j < right.length
    
    result
  end

  # In-place merge sort (more memory efficient)
  def self.sort_in_place(arr, left = 0, right = nil)
    right = arr.length - 1 if right.nil?
    return arr if left >= right
    
    mid = (left + right) / 2
    sort_in_place(arr, left, mid)
    sort_in_place(arr, mid + 1, right)
    merge_in_place(arr, left, mid, right)
    
    arr
  end

  # In-place merge helper
  def self.merge_in_place(arr, left, mid, right)
    # Create temporary arrays
    left_arr = arr[left..mid]
    right_arr = arr[mid+1..right]
    
    i = j = 0
    k = left
    
    # Merge back into original array
    while i < left_arr.length && j < right_arr.length
      if left_arr[i] <= right_arr[j]
        arr[k] = left_arr[i]
        i += 1
      else
        arr[k] = right_arr[j]
        j += 1
      end
      k += 1
    end
    
    # Copy remaining elements
    while i < left_arr.length
      arr[k] = left_arr[i]
      i += 1
      k += 1
    end
    
    while j < right_arr.length
      arr[k] = right_arr[j]
      j += 1
      k += 1
    end
  end

  # Bottom-up merge sort (iterative approach)
  def self.bottom_up_sort(arr)
    return arr if arr.length <= 1
    
    result = arr.dup
    width = 1
    
    while width < result.length
      left = 0
      
      while left < result.length
        mid = [left + width - 1, result.length - 1].min
        right = [left + 2 * width - 1, result.length - 1].min
        
        if mid < right
          merge_bottom_up(result, left, mid, right)
        end
        
        left += 2 * width
      end
      
      width *= 2
    end
    
    result
  end

  # Helper for bottom-up merge
  def self.merge_bottom_up(arr, left, mid, right)
    left_arr = arr[left..mid]
    right_arr = arr[mid+1..right]
    
    i = j = 0
    k = left
    
    while i < left_arr.length && j < right_arr.length
      if left_arr[i] <= right_arr[j]
        arr[k] = left_arr[i]
        i += 1
      else
        arr[k] = right_arr[j]
        j += 1
      end
      k += 1
    end
    
    while i < left_arr.length
      arr[k] = left_arr[i]
      i += 1
      k += 1
    end
    
    while j < right_arr.length
      arr[k] = right_arr[j]
      j += 1
      k += 1
    end
  end
end

# Example usage and testing
if __FILE__ == $0
  puts "Merge Sort Examples:"
  
  unsorted_list = [38, 27, 43, 3, 9, 82, 10]
  puts "Original: #{unsorted_list}"
  puts "Sorted (standard): #{MergeSort.sort(unsorted_list.dup)}"
  puts "Sorted (in-place): #{MergeSort.sort_in_place(unsorted_list.dup)}"
  puts "Sorted (bottom-up): #{MergeSort.bottom_up_sort(unsorted_list.dup)}"
  
  characters = ['m', 'e', 'r', 'g', 'e', 's', 'o', 'r', 't']
  puts "\nSorting characters: #{characters}"
  puts "Result: #{MergeSort.sort(characters).join}"
  
  # Test with different data types
  strings = ["banana", "apple", "cherry", "date"]
  puts "\nSorting strings: #{strings}"
  puts "Result: #{MergeSort.sort(strings)}"
  
  # Test with already sorted array
  sorted_array = [1, 2, 3, 4, 5]
  puts "\nAlready sorted: #{sorted_array}"
  puts "Result: #{MergeSort.sort(sorted_array)}"
  
  # Test with reverse sorted array
  reverse_array = [5, 4, 3, 2, 1]
  puts "\nReverse sorted: #{reverse_array}"
  puts "Result: #{MergeSort.sort(reverse_array)}"
  
  # Performance characteristics
  puts "\nMerge Sort Characteristics:"
  puts "- Time Complexity: O(n log n) in all cases"
  puts "- Space Complexity: O(n) for additional arrays"
  puts "- Stable: Maintains relative order of equal elements"
  puts "- Predictable: Always O(n log n), regardless of input"
end