package Java.fundamentals;

/*
 * Break and Continue Statements
 * Time Complexity: O(n) - depends on loop iterations
 * Space Complexity: O(1) - no additional space required
 *
 * Break: Immediately exits the loop or switch statement.
 * Continue: Skips the current iteration and moves to the next one.
 * These control flow statements provide fine-grained control over loop execution.
 */

public class BreakContinue {

    //break example - find first number divisible by 7
    public static int findFirstDivisibleBy7(int[] numbers) {
        for (int num : numbers) {
            if (num % 7 == 0) {
                System.out.println("Found: " + num);
                break; // Exit loop immediately
            }
            System.out.println("Checking: " + num);
        }
        return -1;
    }

    //continue example - print only even numbers
    public static void printEvenNumbers(int[] numbers) {
        for (int num : numbers) {
            if (num % 2 != 0) {
                continue; //skip odd numbers
            }
            System.out.print(num + " ");
        }
        System.out.println();
    }

    //break in nested loop with label
    public static void findPairWithSum(int[] arr, int targetSum) {
        outerLoop:
        //label for outer loop
        for (int i = 0; i < arr.length; i++) {
            for (int j = i + 1; j < arr.length; j++) {
                if (arr[i] + arr[j] == targetSum) {
                    System.out.println("Pair found: " + arr[i] + " + " + arr[j] + " = " + targetSum);
                    break outerLoop; //break out of both loops
                }
            }
        }
    }

    //continue to skip specific values
    public static int sumExcludingMultiplesOf3(int n) {
        int sum = 0;
        for (int i = 1; i <= n; i++) {
            if (i % 3 == 0) {
                continue; //skip multiples of 3
            }
            sum += i;
        }
        return sum;
    }

    public static void main(String[] args) {
        int[] numbers = {3, 8, 12, 15, 21, 28};

        System.out.println("Finding first number divisible by 7:");
        findFirstDivisibleBy7(numbers);

        System.out.println("\nEven numbers only:");
        printEvenNumbers(numbers);

        System.out.println("\nFinding pair with sum 20:");
        findPairWithSum(numbers, 20);

        System.out.println("\nSum of numbers 1-10 excluding multiples of 3: "
                + sumExcludingMultiplesOf3(10));
    }
}