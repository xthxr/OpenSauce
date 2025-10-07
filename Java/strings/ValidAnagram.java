//  leetcode problem 242
// https://leetcode.com/problems/valid-anagram/

import java.util.Arrays;
public class ValidAnagram { // in leetcode instead of this class Solution is written
    public static boolean isAnagram(String s, String t) {
        String str1 = s.toLowerCase();
        String str2 = t.toLowerCase();
        if (str1.length() == str2.length()) {
            char[] strtochar1 = str1.toCharArray();
            char[] strtochar2 = str2.toCharArray();
            Arrays.sort(strtochar1);
            Arrays.sort(strtochar2);

            return Arrays.equals(strtochar1, strtochar2);
        } else {
            return false;
        }
    }
    public static void main(String[] args) {
        String s = "anagram";
        String t = "nagaram";
        boolean ch = isAnagram(s, t);
        System.out.println(ch); // Output: true
    }
}
