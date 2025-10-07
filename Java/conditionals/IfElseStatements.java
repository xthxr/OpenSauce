package Java.fundamentals;

/*
 * If-Else Conditional Statements
 * Time Complexity: O(1) - constant time for condition evaluation
 * Space Complexity: O(1) - no additional space required
 *
 * If-else statements allow you to execute different code blocks based on conditions.
 * The if block executes when the condition is true, else block executes when false.
 * You can chain multiple conditions using else-if.
 */

public class IfElseStatements {

    //simple if-else example
    public static void checkNumber(int number) {
        if (number > 0) {
            System.out.println(number + " is positive");
        } else if (number < 0) {
            System.out.println(number + " is negative");
        } else {
            System.out.println(number + " is zero");
        }
    }

    //nested if-else example
    public static String gradeEvaluation(int score) {
        if (score >= 0 && score <= 100) {
            if (score >= 90) {
                return "A";
            } else if (score >= 80) {
                return "B";
            } else if (score >= 70) {
                return "C";
            } else if (score >= 60) {
                return "D";
            } else {
                return "F";
            }
        } else {
            return "Invalid score";
        }
    }

    public static void main(String[] args) {
        checkNumber(10);
        checkNumber(-5);
        checkNumber(0);

        System.out.println("Grade for 85: " + gradeEvaluation(85));
        System.out.println("Grade for 72: " + gradeEvaluation(72));
    }
}