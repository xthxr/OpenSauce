/**
 * Bubble Sort Algorithm
 * 
 * Time Complexity: O(n^2) in worst and average case, O(n) in best case (when array is already sorted)
 * Space Complexity: O(1)
 * 
 * Bubble sort is a simple sorting algorithm that repeatedly steps through the list,
 * compares adjacent elements and swaps them if they are in the wrong order.
 */
public class BubbleSort {
    
    /**
     * Sorts an array using bubble sort algorithm (ascending order).
     * 
     * @param arr Array of integers to be sorted
     * @return Sorted array
     */
    public static int[] bubbleSort(int[] arr) {
        int n = arr.length;
        
        // Traverse through all array elements
        for (int i = 0; i < n; i++) {
            boolean swapped = false;
            
            // Last i elements are already in place
            for (int j = 0; j < n - i - 1; j++) {
                // Swap if the element found is greater than the next element
                if (arr[j] > arr[j + 1]) {
                    int temp = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = temp;
                    swapped = true;
                }
            }
            
            // If no swapping happened, array is already sorted
            if (!swapped) {
                break;
            }
        }
        
        return arr;
    }
    
    /**
     * Sorts an array in descending order using bubble sort.
     * 
     * @param arr Array of integers to be sorted
     * @return Sorted array in descending order
     */
    public static int[] bubbleSortDescending(int[] arr) {
        int n = arr.length;
        
        for (int i = 0; i < n; i++) {
            boolean swapped = false;
            
            for (int j = 0; j < n - i - 1; j++) {
                // Swap if the element found is smaller than the next element
                if (arr[j] < arr[j + 1]) {
                    int temp = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = temp;
                    swapped = true;
                }
            }
            
            if (!swapped) {
                break;
            }
        }
        
        return arr;
    }
    
    /**
     * Utility method to print an array
     */
    public static void printArray(int[] arr) {
        for (int i = 0; i < arr.length; i++) {
            System.out.print(arr[i] + " ");
        }
        System.out.println();
    }
    
    /**
     * Main method with example usage
     */
    public static void main(String[] args) {
        System.out.println("Bubble Sort Examples:");
        
        // Test ascending sort
        int[] arr1 = {64, 34, 25, 12, 22, 11, 90};
        System.out.print("\nOriginal array: ");
        printArray(arr1);
        
        bubbleSort(arr1);
        System.out.print("Sorted array (ascending): ");
        printArray(arr1);
        
        // Test descending sort
        int[] arr2 = {64, 34, 25, 12, 22, 11, 90};
        bubbleSortDescending(arr2);
        System.out.print("Sorted array (descending): ");
        printArray(arr2);
        
        // Test with already sorted array
        int[] arr3 = {1, 2, 3, 4, 5};
        System.out.print("\nAlready sorted array: ");
        printArray(arr3);
        bubbleSort(arr3);
        System.out.print("After bubble sort: ");
        printArray(arr3);
        
        // Test with single element
        int[] arr4 = {42};
        System.out.print("\nSingle element array: ");
        printArray(arr4);
        bubbleSort(arr4);
        System.out.print("After bubble sort: ");
        printArray(arr4);
    }
}
