import java.util.HashSet;
import java.util.HashMap;
import java.util.ArrayList;

/**
 * Subarray with Zero Sum
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(n)
 * 
 * Check if there exists a subarray with sum equal to zero.
 * Uses prefix sum and HashSet technique.
 */
public class SubarrayWithZeroSum {
    
    /**
     * Checks if there exists a subarray with sum zero.
     * 
     * @param arr Array of integers
     * @return true if subarray with zero sum exists, false otherwise
     */
    public static boolean hasZeroSumSubarray(int[] arr) {
        if (arr == null || arr.length == 0) {
            return false;
        }
        
        HashSet<Integer> prefixSums = new HashSet<>();
        int currentSum = 0;
        
        for (int num : arr) {
            currentSum += num;
            
            // If current sum is 0 or already seen, we found a zero sum subarray
            if (currentSum == 0 || prefixSums.contains(currentSum)) {
                return true;
            }
            
            prefixSums.add(currentSum);
        }
        
        return false;
    }
    
    /**
     * Finds all subarrays with zero sum.
     * 
     * @param arr Array of integers
     * @return List of all zero-sum subarrays as [start, end] pairs
     */
    public static ArrayList<int[]> findAllZeroSumSubarrays(int[] arr) {
        ArrayList<int[]> result = new ArrayList<>();
        if (arr == null || arr.length == 0) {
            return result;
        }
        
        HashMap<Integer, ArrayList<Integer>> sumIndices = new HashMap<>();
        sumIndices.put(0, new ArrayList<>());
        sumIndices.get(0).add(-1);
        
        int currentSum = 0;
        
        for (int i = 0; i < arr.length; i++) {
            currentSum += arr[i];
            
            if (sumIndices.containsKey(currentSum)) {
                for (int startIdx : sumIndices.get(currentSum)) {
                    result.add(new int[]{startIdx + 1, i});
                }
            }
            
            if (!sumIndices.containsKey(currentSum)) {
                sumIndices.put(currentSum, new ArrayList<>());
            }
            sumIndices.get(currentSum).add(i);
        }
        
        return result;
    }
    
    /**
     * Main method with example usage and test cases
     */
    public static void main(String[] args) {
        System.out.println("=== Subarray with Zero Sum ===\n");
        
        // Test case 1
        int[] arr1 = {4, 2, -3, 1, 6};
        System.out.println("Test 1: [4, 2, -3, 1, 6]");
        System.out.println("Has zero sum subarray: " + hasZeroSumSubarray(arr1) + "\n");
        
        // Test case 2
        int[] arr2 = {4, 2, 0, 1, 6};
        System.out.println("Test 2: [4, 2, 0, 1, 6]");
        System.out.println("Has zero sum subarray: " + hasZeroSumSubarray(arr2));
        ArrayList<int[]> subarrays2 = findAllZeroSumSubarrays(arr2);
        System.out.println("Zero sum subarrays:");
        for (int[] sub : subarrays2) {
            System.out.println("  [" + sub[0] + ", " + sub[1] + "]");
        }
        System.out.println();
        
        // Test case 3
        int[] arr3 = {-3, 2, 3, 1, 6};
        System.out.println("Test 3: [-3, 2, 3, 1, 6]");
        System.out.println("Has zero sum subarray: " + hasZeroSumSubarray(arr3));
        ArrayList<int[]> subarrays3 = findAllZeroSumSubarrays(arr3);
        System.out.println("Zero sum subarrays:");
        for (int[] sub : subarrays3) {
            System.out.print("  [" + sub[0] + ", " + sub[1] + "] -> [");
            for (int i = sub[0]; i <= sub[1]; i++) {
                System.out.print(arr3[i] + (i < sub[1] ? ", " : ""));
            }
            System.out.println("]");
        }
    }
}
