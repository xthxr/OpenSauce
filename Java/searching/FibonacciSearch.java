
// FibonacciSearch.java
// Implementation of Fibonacci Search in Java
// Time Complexity: O(log n)
// Space Complexity: O(1)

public class FibonacciSearch {

    // Returns index of x if present in arr[], else -1
    public static int fibonacciSearch(int[] arr, int x) {
        int n = arr.length;

        // Initialize fibonacci numbers
        int fibMMm2 = 0; // (m-2)'th Fibonacci number
        int fibMMm1 = 1; // (m-1)'th Fibonacci number
        int fibM = fibMMm2 + fibMMm1; // m'th Fibonacci number

        // fibM is going to store the smallest Fibonacci number >= n
        while (fibM < n) {
            fibMMm2 = fibMMm1;
            fibMMm1 = fibM;
            fibM = fibMMm2 + fibMMm1;
        }

        // Marks the eliminated range from front
        int offset = -1;

        // While there are elements to be inspected
        while (fibM > 1) {
            // Check if fibMMm2 is a valid location
            int i = Math.min(offset + fibMMm2, n - 1);

            if (arr[i] < x) {
                fibM = fibMMm1;
                fibMMm1 = fibMMm2;
                fibMMm2 = fibM - fibMMm1;
                offset = i;
            } else if (arr[i] > x) {
                fibM = fibMMm2;
                fibMMm1 = fibMMm1 - fibMMm2;
                fibMMm2 = fibM - fibMMm1;
            } else {
                return i;
            }
        }

        // Compare the last element with x
        if (fibMMm1 == 1 && offset + 1 < n && arr[offset + 1] == x)
            return offset + 1;

        // Element not found
        return -1;
    }

    // Example usage
    public static void main(String[] args) {
        int[] arr = {10, 22, 35, 40, 45, 50, 80, 82, 85, 90,95, 100};
        int target = 95;

        int result = fibonacciSearch(arr, target);

        if (result >= 0)
            System.out.println("Element found at index: " + result);
        else
            System.out.println("Element not found");
    }
}

