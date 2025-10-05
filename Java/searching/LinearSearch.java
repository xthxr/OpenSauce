/**
 * Linear Search Algorithm
 * 
 * Time Complexity: O(n)
 * Space Complexity: O(1)
 * 
 * Linear search is a simple algorithm for finding a target value in an array.
 * It works by checking each element of the array one by one until the target is found.
 */
public class LinearSearch {
    
    /**
     * Performs iterative linear search on an array.
     * 
     * @param arr Array of integers
     * @param target Element to search for
     * @return Index of target if found, -1 otherwise
     */
    public static int linearSearch(int[] arr, int target) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == target) {
                return i;  // Element found
            }
        }
        return -1;  // Element not found
    }
    
    /**
     * Performs recursive linear search on an array.
     * 
     * @param arr Array of integers
     * @param target Element to search for
     * @param index Current index being checked
     * @return Index of target if found, -1 otherwise
     */
    public static int linearSearchRecursive(int[] arr, int target, int index) {
        if (index >= arr.length) {
            return -1;  // Base case: element not found
        }
        
        if (arr[index] == target) {
            return index;
        }
        
        return linearSearchRecursive(arr, target, index + 1);
    }
    
    /**
     * Main method with example usage
     */
    public static void main(String[] args) {
        int[] testArray = {4, 2, 9, 7, 5, 1, 8, 6, 3, 0};
        
        // Test iterative version
        System.out.println("Iterative Linear Search:");
        int target = 7;
        int result = linearSearch(testArray, target);
        if (result != -1) {
            System.out.println("Element " + target + " found at index " + result);
        } else {
            System.out.println("Element " + target + " not found in array");
        }
        
        // Test recursive version
        System.out.println("\nRecursive Linear Search:");
        target = 3;
        result = linearSearchRecursive(testArray, target, 0);
        if (result != -1) {
            System.out.println("Element " + target + " found at index " + result);
        } else {
            System.out.println("Element " + target + " not found in array");
        }
        
        // Test with element not in array
        target = 10;
        result = linearSearch(testArray, target);
        if (result != -1) {
            System.out.println("\nElement " + target + " found at index " + result);
        } else {
            System.out.println("\nElement " + target + " not found in array");
        }
    }
}
