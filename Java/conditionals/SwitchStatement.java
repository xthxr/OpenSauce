package Java.fundamentals;

/*
 * Switch Statement
 * Time Complexity: O(1) - constant time lookup (typically implemented as jump table)
 * Space Complexity: O(1) - no additional space required
 *
 * Switch statements provide a cleaner way to handle multiple conditions based on a single variable.
 * More efficient than multiple if-else statements when checking equality of a single variable.
 * Supports int, char, String, and enum types.
 */

public class SwitchStatement {

    //basic switch example with int
    public static String getDayName(int day) {
        String dayName;
        switch (day) {
            case 1:
                dayName = "Monday";
                break;
            case 2:
                dayName = "Tuesday";
                break;
            case 3:
                dayName = "Wednesday";
                break;
            case 4:
                dayName = "Thursday";
                break;
            case 5:
                dayName = "Friday";
                break;
            case 6:
                dayName = "Saturday";
                break;
            case 7:
                dayName = "Sunday";
                break;
            default:
                dayName = "Invalid day";
                break;
        }
        return dayName;
    }

    //switch with String
    public static int getMonthDays(String month) {
        int days;
        switch (month.toLowerCase()) {
            case "january":
            case "march":
            case "may":
            case "july":
            case "august":
            case "october":
            case "december":
                days = 31;
                break;
            case "april":
            case "june":
            case "september":
            case "november":
                days = 30;
                break;
            case "february":
                days = 28; //not considering leap year
                break;
            default:
                days = -1;
                break;
        }
        return days;
    }

    public static void main(String[] args) {
        System.out.println("Day 3: " + getDayName(3));
        System.out.println("Day 7: " + getDayName(7));

        System.out.println("January has " + getMonthDays("January") + " days");
        System.out.println("February has " + getMonthDays("February") + " days");
    }
}