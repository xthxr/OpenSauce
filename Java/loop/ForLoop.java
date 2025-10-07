package Java.fundamentals;

/*
 * For Loop
 * Time Complexity: O(n) - where n is the number of iterations
 * Space Complexity: O(1) - no additional space required
 *
 * For loops are used when you know the number of iterations in advance.
 * Consists of three parts: initialization, condition, and increment/decrement.
 * Most commonly used loop for iterating over arrays and collections.
 */

public class ForLoop {

    //basic for loop - print numbers 1 to 10
    public static void printNumbers() {
        for (int i = 1; i <= 10; i++) {
            System.out.print(i + " ");
        }
        System.out.println();
    }

    //for loop with array
    public static int sumArray(int[] arr) {
        int sum = 0;
        for (int i = 0; i < arr.length; i++) {
            sum += arr[i];
        }
        return sum;
    }

    //enhanced for loop (for-each)
    public static void printArray(int[] arr) {
        for (int num : arr) {
            System.out.print(num + " ");
        }
        System.out.println();
    }

    //nested for loop - multiplication table
    public static void multiplicationTable(int n) {
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= n; j++) {
                System.out.printf("%4d", i * j);
            }
            System.out.println();
        }
    }

    public static void main(String[] args) {
        System.out.println("Numbers 1 to 10:");
        printNumbers();

        int[] numbers = {5, 10, 15, 20, 25};
        System.out.println("Sum of array: " + sumArray(numbers));

        System.out.println("Array elements:");
        printArray(numbers);

        System.out.println("\n5x5 Multiplication Table:");
        multiplicationTable(5);
    }
}