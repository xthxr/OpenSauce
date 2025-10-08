# Two Sum problem implementation in Ruby
# Given an array of integers and a target sum, return indices of two numbers that add up to target
# Time Complexity: O(n²) for brute force, O(n) for hash map approach
# Space Complexity: O(1) for brute force, O(n) for hash map approach

class TwoSum
  # Brute force approach - check all pairs
  # Time Complexity: O(n²), Space Complexity: O(1)
  def self.brute_force(nums, target)
    (0...nums.length).each do |i|
      (i + 1...nums.length).each do |j|
        return [i, j] if nums[i] + nums[j] == target
      end
    end
    nil
  end

  # Hash map approach using Hash
  # Time Complexity: O(n), Space Complexity: O(n)
  def self.hash_map(nums, target)
    seen = {}
    nums.each_with_index do |num, idx|
      complement = target - num
      return [seen[complement], idx] if seen.key?(complement)
      seen[num] = idx
    end
    nil
  end

  # Find all pairs that sum to target
  # Time Complexity: O(n²), Space Complexity: O(n)
  def self.all_pairs(nums, target)
    pairs = []
    (0...nums.length).each do |i|
      (i + 1...nums.length).each do |j|
        pairs << [i, j] if nums[i] + nums[j] == target
      end
    end
    pairs
  end

  # Two sum with sorted array (two pointers approach)
  # Time Complexity: O(n), Space Complexity: O(1)
  # Assumes input array is sorted
  def self.sorted_array(nums, target)
    left = 0
    right = nums.length - 1
    
    while left < right
      sum = nums[left] + nums[right]
      return [left, right] if sum == target
      if sum < target
        left += 1
      else
        right -= 1
      end
    end
    nil
  end
end

# Example usage and testing
if __FILE__ == $0
  puts "Two Sum Examples:"
  
  nums1 = [2, 7, 11, 15]
  target1 = 9
  puts "Array: #{nums1}, Target: #{target1}"
  puts "Brute Force: #{TwoSum.brute_force(nums1, target1)}"
  puts "Hash Map: #{TwoSum.hash_map(nums1, target1)}"
  
  nums2 = [3, 2, 4]
  target2 = 6
  puts "\nArray: #{nums2}, Target: #{target2}"
  puts "Brute Force: #{TwoSum.brute_force(nums2, target2)}"
  puts "Hash Map: #{TwoSum.hash_map(nums2, target2)}"
  puts "All pairs for target #{target2}: #{TwoSum.all_pairs(nums2, target2)}"
  
  sorted_nums = [1, 2, 3, 4, 6]
  puts "\nSorted array: #{sorted_nums}, Target: 6"
  puts "Two pointers: #{TwoSum.sorted_array(sorted_nums, 6)}"
end