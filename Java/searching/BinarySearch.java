/**
 * Binary Search Algorithm
 * 
 * Time Complexity: O(log n)
 * Space Complexity: O(1) for iterative, O(log n) for recursive (due to call stack)
 * 
 * Binary search is an efficient algorithm for finding a target value in a sorted array.
 * It works by repeatedly dividing the search interval in half.
 */
public class BinarySearch {
    
    /**
     * Performs iterative binary search on a sorted array.
     * 
     * @param arr Sorted array of integers
     * @param target Element to search for
     * @return Index of target if found, -1 otherwise
     */
    public static int binarySearch(int[] arr, int target) {
        int left = 0;
        int right = arr.length - 1;
        
        while (left <= right) {
            int mid = left + (right - left) / 2;  // Avoids potential overflow
            
            if (arr[mid] == target) {
                return mid;
            } else if (arr[mid] < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        
        return -1;  // Element not found
    }
    
    /**
     * Performs recursive binary search on a sorted array.
     * 
     * @param arr Sorted array of integers
     * @param target Element to search for
     * @param left Left boundary of search
     * @param right Right boundary of search
     * @return Index of target if found, -1 otherwise
     */
    public static int binarySearchRecursive(int[] arr, int target, int left, int right) {
        if (left > right) {
            return -1;  // Base case: element not found
        }
        
        int mid = left + (right - left) / 2;
        
        if (arr[mid] == target) {
            return mid;
        } else if (arr[mid] < target) {
            return binarySearchRecursive(arr, target, mid + 1, right);
        } else {
            return binarySearchRecursive(arr, target, left, mid - 1);
        }
    }
    
    /**
     * Main method with example usage
     */
    public static void main(String[] args) {
        int[] testArray = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19};
        
        // Test iterative version
        System.out.println("Iterative Binary Search:");
        int target = 7;
        int result = binarySearch(testArray, target);
        if (result != -1) {
            System.out.println("Element " + target + " found at index " + result);
        } else {
            System.out.println("Element " + target + " not found in array");
        }
        
        // Test recursive version
        System.out.println("\nRecursive Binary Search:");
        target = 13;
        result = binarySearchRecursive(testArray, target, 0, testArray.length - 1);
        if (result != -1) {
            System.out.println("Element " + target + " found at index " + result);
        } else {
            System.out.println("Element " + target + " not found in array");
        }
        
        // Test with element not in array
        target = 8;
        result = binarySearch(testArray, target);
        if (result != -1) {
            System.out.println("\nElement " + target + " found at index " + result);
        } else {
            System.out.println("\nElement " + target + " not found in array");
        }
    }
}
