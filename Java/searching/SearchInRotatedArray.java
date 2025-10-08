/**
 * This class provides a solution for searching an element in a rotated sorted array.
 * A rotated sorted array is a sorted array that has been rotated at some pivot point.
 * Example: Original array: [1,2,3,4,5] -> Rotated array: [3,4,5,1,2]
 * 
 * Time Complexity: O(log n) - Modified Binary Search approach
 * Space Complexity: O(1) - Constant extra space
 */
public class SearchInRotatedArray {
    
    /**
     * Searches for a target value in a rotated sorted array.
     * The array must have been originally sorted in ascending order and then rotated.
     * 
     * @param nums the rotated sorted array to search in
     * @param target the value to search for
     * @return the index of target if found, -1 otherwise
     */
    public int search(int[] nums, int target) {
        int start = 0;
        int end = nums.length - 1;
        
        while (start <= end) {
            int mid = start + (end - start) / 2;  // Prevents integer overflow
            
            // Found the target
            if (target == nums[mid]) {
                return mid;
            }
            
            // Check if left half is sorted
            if (nums[start] <= nums[mid]) {
                // Check if target lies in the left sorted half
                if (nums[start] <= target && target <= nums[mid]) {
                    end = mid - 1;  // Search in left half
                } else {
                    start = mid + 1;  // Search in right half
                }
            }
            // Right half must be sorted
            else {
                // Check if target lies in the right sorted half
                if (nums[mid] <= target && target <= nums[end]) {
                    start = mid + 1;  // Search in right half
                } else {
                    end = mid - 1;  // Search in left half
                }
            }
        }
        
        return -1;  // Target not found
    }

    /**
     * Main method to test the search functionality with example cases.
     * 
     * @param args command line arguments (not used)
     */
    public static void main(String[] args) {
        SearchInRotatedArray solution = new SearchInRotatedArray();
        
        // Test case 1: Standard rotated array
        int[] nums1 = {4, 5, 6, 7, 0, 1, 2};
        int target1 = 0;
        System.out.println("Test case 1:");
        System.out.println("Array: [4, 5, 6, 7, 0, 1, 2]");
        System.out.println("Target: " + target1);
        System.out.println("Found at index: " + solution.search(nums1, target1));
        System.out.println();
        
        // Test case 2: Target not in array
        int[] nums2 = {4, 5, 6, 7, 0, 1, 2};
        int target2 = 3;
        System.out.println("Test case 2:");
        System.out.println("Array: [4, 5, 6, 7, 0, 1, 2]");
        System.out.println("Target: " + target2);
        System.out.println("Found at index: " + solution.search(nums2, target2));
        System.out.println();
        
        // Test case 3: Array not rotated (or rotated by length)
        int[] nums3 = {1, 2, 3, 4, 5};
        int target3 = 3;
        System.out.println("Test case 3:");
        System.out.println("Array: [1, 2, 3, 4, 5]");
        System.out.println("Target: " + target3);
        System.out.println("Found at index: " + solution.search(nums3, target3));
        System.out.println();
        
        // Test case 4: Single element array
        int[] nums4 = {1};
        int target4 = 1;
        System.out.println("Test case 4:");
        System.out.println("Array: [1]");
        System.out.println("Target: " + target4);
        System.out.println("Found at index: " + solution.search(nums4, target4));
    }
}
