package Java.fundamentals;

/*
 * Do-While Loop
 * Time Complexity: O(n) - where n is the number of iterations
 * Space Complexity: O(1) - no additional space required
 *
 * Do-while loops are similar to while loops, but the condition is checked after each iteration.
 * This guarantees that the loop body executes at least once.
 * Useful when you need to execute code before checking the condition.
 */

public class DoWhileLoop {

    //basic do-while loop
    public static void printNumbersReverse(int n) {
        do {
            System.out.print(n + " ");
            n--;
        } while (n > 0);
        System.out.println();
    }

    //do-while for input validation (simulated)
    public static int validatePositiveNumber(int input) {
        int attempts = 0;
        do {
            attempts++;
            if (input > 0) {
                System.out.println("Valid input: " + input);
                break;
            } else {
                System.out.println("Invalid input. Attempt " + attempts);
                input = 10; //simulating corrected input
            }
        } while (input <= 0);

        return attempts;
    }

    //do-while to calculate sum of digits
    public static int sumOfDigits(int number) {
        int sum = 0;
        number = Math.abs(number); //handle negative numbers

        do {
            sum += number % 10;
            number /= 10;
        } while (number > 0);

        return sum;
    }

    public static void main(String[] args) {
        System.out.println("Numbers from 5 to 1:");
        printNumbersReverse(5);

        System.out.println("\nValidation test:");
        validatePositiveNumber(-5);

        System.out.println("\nSum of digits of 12345: " + sumOfDigits(12345));
        System.out.println("Sum of digits of 999: " + sumOfDigits(999));
    }
}