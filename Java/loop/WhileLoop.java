package Java.fundamentals;

/*
 * While Loop
 * Time Complexity: O(n) - where n is the number of iterations
 * Space Complexity: O(1) - no additional space required
 *
 * While loops are used when the number of iterations is not known in advance.
 * The condition is checked before each iteration.
 * Loop continues as long as the condition is true.
 */

public class WhileLoop {

    //basic while loop - countdown
    public static void countdown(int start) {
        while (start > 0) {
            System.out.print(start + " ");
            start--;
        }
        System.out.println("Blast off!");
    }

    //while loop to find factorial
    public static long factorial(int n) {
        long result = 1;
        int i = 1;
        while (i <= n) {
            result *= i;
            i++;
        }
        return result;
    }

    //while loop with user input simulation
    public static int sumUntilZero(int[] numbers) {
        int sum = 0;
        int index = 0;

        while (index < numbers.length && numbers[index] != 0) {
            sum += numbers[index];
            index++;
        }
        return sum;
    }

    public static void main(String[] args) {
        System.out.println("Countdown from 5:");
        countdown(5);

        System.out.println("\nFactorial of 5: " + factorial(5));

        int[] numbers = {10, 20, 30, 0, 40, 50};
        System.out.println("Sum until zero: " + sumUntilZero(numbers));
    }
}