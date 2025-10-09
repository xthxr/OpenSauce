/**
 * This class finds a peak element in an array where a peak element is defined as
 * an element that is strictly greater than its neighbors.
 * Note: For edge elements, we only need to compare with the inner neighbor.
 * 
 * Example:
 * Input: nums = [1,2,3,1]
 * Output: 2 (3 is a peak element because it's greater than its neighbors)
 * 
 * Time Complexity: O(log n) - Binary Search approach
 * Space Complexity: O(1) - Constant extra space
 */
public class FindPeakElement {
    
    /**
     * Finds a peak element in the array using binary search.
     * The function assumes that nums[i] != nums[i + 1] for all valid i.
     * 
     * @param nums the input array to find peak element in
     * @return the index of any peak element in the array
     */
    public int findPeakElement(int[] nums) {
        // Edge case: single element array
        if (nums.length == 1) {
            return 0;
        }
        
        int low = 0;
        int high = nums.length - 1;
        
        while (low < high) {
            int mid = low + (high - low) / 2;  // Prevents integer overflow
            
            // If mid element is greater than next element,
            // a peak exists in the left half (including mid)
            if (nums[mid] > nums[mid + 1]) {
                high = mid;
            }
            // If mid element is less than next element,
            // a peak exists in the right half
            else {
                low = mid + 1;
            }
        }
        
        // At this point, low == high and points to the peak element
        return low;
    }

    /**
     * Main method to test the peak finding functionality with example cases.
     * 
     * @param args command line arguments (not used)
     */
    public static void main(String[] args) {
        FindPeakElement solution = new FindPeakElement();
        
        // Test case 1: Standard case with peak in middle
        int[] nums1 = {1, 2, 3, 1};
        System.out.println("Test case 1:");
        System.out.println("Array: [1, 2, 3, 1]");
        int peak1 = solution.findPeakElement(nums1);
        System.out.println("Peak element index: " + peak1);
        System.out.println("Peak element value: " + nums1[peak1]);
        System.out.println();
        
        // Test case 2: Multiple peaks (returns any peak)
        int[] nums2 = {1, 2, 1, 3, 5, 6, 4};
        System.out.println("Test case 2:");
        System.out.println("Array: [1, 2, 1, 3, 5, 6, 4]");
        int peak2 = solution.findPeakElement(nums2);
        System.out.println("Peak element index: " + peak2);
        System.out.println("Peak element value: " + nums2[peak2]);
        System.out.println();
        
        // Test case 3: Peak at the end
        int[] nums3 = {1, 2, 3, 4};
        System.out.println("Test case 3:");
        System.out.println("Array: [1, 2, 3, 4]");
        int peak3 = solution.findPeakElement(nums3);
        System.out.println("Peak element index: " + peak3);
        System.out.println("Peak element value: " + nums3[peak3]);
        System.out.println();
        
        // Test case 4: Single element array
        int[] nums4 = {1};
        System.out.println("Test case 4:");
        System.out.println("Array: [1]");
        int peak4 = solution.findPeakElement(nums4);
        System.out.println("Peak element index: " + peak4);
        System.out.println("Peak element value: " + nums4[peak4]);
    }
}