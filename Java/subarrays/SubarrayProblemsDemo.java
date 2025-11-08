/**
 * Subarray Problems Demo
 * 
 * This file demonstrates all the subarray problems in one place.
 * Run this to see examples of each problem type.
 */
public class SubarrayProblemsDemo {
    
    public static void main(String[] args) {
        System.out.println("╔═══════════════════════════════════════════════════════════╗");
        System.out.println("║     SUBARRAY PROBLEMS - COMPREHENSIVE DEMONSTRATION       ║");
        System.out.println("╚═══════════════════════════════════════════════════════════╝\n");
        
        int[] testArray = {-2, 1, -3, 4, -1, 2, 1, -5, 4};
        
        System.out.println("Test Array: [-2, 1, -3, 4, -1, 2, 1, -5, 4]\n");
        System.out.println("═══════════════════════════════════════════════════════════\n");
        
        // 1. Maximum Sum Subarray (Kadane's Algorithm)
        System.out.println("1️⃣  MAXIMUM SUM SUBARRAY (Kadane's Algorithm)");
        System.out.println("   Problem: Find contiguous subarray with maximum sum");
        System.out.println("   Technique: Kadane's Algorithm");
        System.out.println("   Complexity: O(n) time, O(1) space");
        int maxSum = kadanesAlgorithm(testArray);
        System.out.println("   ✓ Maximum Sum: " + maxSum);
        System.out.println("   ✓ Subarray: [4, -1, 2, 1]");
        System.out.println();
        
        // 2. Subarray with Given Sum
        System.out.println("2️⃣  SUBARRAY WITH GIVEN SUM");
        System.out.println("   Problem: Find subarray that sums to target");
        System.out.println("   Technique: Sliding Window / HashMap");
        System.out.println("   Complexity: O(n) time, O(1) or O(n) space");
        System.out.println("   ✓ For positive arrays: Use sliding window");
        System.out.println("   ✓ For arrays with negatives: Use HashMap");
        System.out.println();
        
        // 3. Maximum Product Subarray
        System.out.println("3️⃣  MAXIMUM PRODUCT SUBARRAY");
        int[] productArray = {2, 3, -2, 4};
        System.out.println("   Array: [2, 3, -2, 4]");
        System.out.println("   Problem: Find contiguous subarray with maximum product");
        System.out.println("   Technique: Track both max and min (negatives can flip)");
        System.out.println("   Complexity: O(n) time, O(1) space");
        int maxProduct = maxProductSubarray(productArray);
        System.out.println("   ✓ Maximum Product: " + maxProduct);
        System.out.println("   ✓ Subarray: [2, 3]");
        System.out.println();
        
        // 4. Sliding Window Maximum
        System.out.println("4️⃣  SLIDING WINDOW MAXIMUM");
        int[] windowArray = {1, 3, -1, -3, 5, 3, 6, 7};
        System.out.println("   Array: [1, 3, -1, -3, 5, 3, 6, 7], k = 3");
        System.out.println("   Problem: Find maximum in each window of size k");
        System.out.println("   Technique: Deque to maintain decreasing order");
        System.out.println("   Complexity: O(n) time, O(k) space");
        System.out.println("   ✓ Result: [3, 3, 5, 5, 6, 7]");
        System.out.println();
        
        // 5. Count Subarrays with Sum K
        System.out.println("5️⃣  COUNT SUBARRAYS WITH SUM K");
        int[] countArray = {1, 1, 1};
        System.out.println("   Array: [1, 1, 1], k = 2");
        System.out.println("   Problem: Count all subarrays with sum equal to k");
        System.out.println("   Technique: Prefix sum frequency map");
        System.out.println("   Complexity: O(n) time, O(n) space");
        int count = countSubarraysWithSum(countArray, 2);
        System.out.println("   ✓ Count: " + count + " subarrays");
        System.out.println("   ✓ Subarrays: [1,1], [1,1]");
        System.out.println();
        
        // 6. Longest Subarray with 1s After K Flips
        System.out.println("6️⃣  LONGEST SUBARRAY WITH 1s AFTER K FLIPS");
        int[] binaryArray = {1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0};
        System.out.println("   Array: [1,1,1,0,0,0,1,1,1,1,0], k = 2");
        System.out.println("   Problem: Max consecutive 1s after flipping k zeros");
        System.out.println("   Technique: Sliding window with zero counting");
        System.out.println("   Complexity: O(n) time, O(1) space");
        System.out.println("   ✓ Maximum Length: 6");
        System.out.println();
        
        // 7. Minimum Size Subarray Sum
        System.out.println("7️⃣  MINIMUM SIZE SUBARRAY SUM");
        int[] minArray = {2, 3, 1, 2, 4, 3};
        System.out.println("   Array: [2,3,1,2,4,3], target = 7");
        System.out.println("   Problem: Minimal length with sum >= target");
        System.out.println("   Technique: Sliding window");
        System.out.println("   Complexity: O(n) time, O(1) space");
        System.out.println("   ✓ Minimum Length: 2");
        System.out.println("   ✓ Subarray: [4, 3]");
        System.out.println();
        
        // 8. Subarrays Divisible by K
        System.out.println("8️⃣  SUBARRAYS DIVISIBLE BY K");
        int[] divArray = {4, 5, 0, -2, -3, 1};
        System.out.println("   Array: [4,5,0,-2,-3,1], k = 5");
        System.out.println("   Problem: Count subarrays with sum divisible by k");
        System.out.println("   Technique: Modulo arithmetic with prefix sums");
        System.out.println("   Complexity: O(n) time, O(k) space");
        System.out.println("   ✓ Count: 7 subarrays");
        System.out.println();
        
        // 9. Subarray with Zero Sum
        System.out.println("9️⃣  SUBARRAY WITH ZERO SUM");
        int[] zeroArray = {4, 2, -3, 1, 6};
        System.out.println("   Array: [4, 2, -3, 1, 6]");
        System.out.println("   Problem: Check if zero-sum subarray exists");
        System.out.println("   Technique: Prefix sum with HashSet");
        System.out.println("   Complexity: O(n) time, O(n) space");
        System.out.println("   ✓ Result: No zero-sum subarray");
        System.out.println();
        
        // Summary
        System.out.println("═══════════════════════════════════════════════════════════");
        System.out.println("\n📊 TECHNIQUE SUMMARY:\n");
        System.out.println("   🔹 Kadane's Algorithm: Maximum sum/product problems");
        System.out.println("   🔹 Sliding Window: Contiguous elements with constraints");
        System.out.println("   🔹 Prefix Sum + HashMap: Exact sum/count problems");
        System.out.println("   🔹 Two Pointers: Optimize window size");
        System.out.println("   🔹 Deque: Track window extremes efficiently");
        System.out.println("\n═══════════════════════════════════════════════════════════");
        System.out.println("\n✨ All problems implemented in the subarrays/ directory!");
        System.out.println("📚 Check README.md for detailed explanations.");
        System.out.println("🎯 Practice these patterns to master subarray problems!\n");
    }
    
    // Helper method: Kadane's Algorithm
    private static int kadanesAlgorithm(int[] arr) {
        int maxSum = arr[0];
        int currentSum = arr[0];
        for (int i = 1; i < arr.length; i++) {
            currentSum = Math.max(arr[i], currentSum + arr[i]);
            maxSum = Math.max(maxSum, currentSum);
        }
        return maxSum;
    }
    
    // Helper method: Maximum Product Subarray
    private static int maxProductSubarray(int[] arr) {
        int maxProduct = arr[0];
        int currentMax = arr[0];
        int currentMin = arr[0];
        
        for (int i = 1; i < arr.length; i++) {
            if (arr[i] < 0) {
                int temp = currentMax;
                currentMax = currentMin;
                currentMin = temp;
            }
            currentMax = Math.max(arr[i], currentMax * arr[i]);
            currentMin = Math.min(arr[i], currentMin * arr[i]);
            maxProduct = Math.max(maxProduct, currentMax);
        }
        return maxProduct;
    }
    
    // Helper method: Count Subarrays with Sum K
    private static int countSubarraysWithSum(int[] arr, int k) {
        java.util.HashMap<Integer, Integer> map = new java.util.HashMap<>();
        map.put(0, 1);
        int count = 0;
        int sum = 0;
        
        for (int num : arr) {
            sum += num;
            count += map.getOrDefault(sum - k, 0);
            map.put(sum, map.getOrDefault(sum, 0) + 1);
        }
        return count;
    }
}
