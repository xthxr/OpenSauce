//  This is Leetcode based DSA question --
// ✅ Problem Statement

/*
Roman to Integer

Time Complexity: O(n)     // n = length of the Roman string
Space Complexity: O(1)

Converts a Roman numeral string to its integer representation.
*/

import java.util.HashMap;


public class RomanToInteger {

    /**
     * Converts a Roman numeral string to an integer.
     *
     * @param s Roman numeral string
     * @return  Equivalent integer value
     */
    public int romanToInt(String s) {
        HashMap<Character, Integer> map = new HashMap<>();
        map.put('I', 1);
        map.put('V', 5);
        map.put('X', 10);
        map.put('L', 50);
        map.put('C', 100);
        map.put('D', 500);
        map.put('M', 1000);

        int total = 0;

        for (int i = 0; i < s.length(); i++) {
            int value = map.get(s.charAt(i));
            if (i < s.length() - 1 && value < map.get(s.charAt(i + 1))) {
                total -= value;
            } else {
                total += value;
            }
        }

        return total;
    }

    // Example usage
    public static void main(String[] args) {
        RomanToInteger converter = new RomanToInteger();
        String roman = "MCMXCIV";

        System.out.println("Roman: " + roman + " -> Integer: " + converter.romanToInt(roman));
    }
}
