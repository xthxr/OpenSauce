
// KMPPatternMatching.java
// Implementation of KMP (Knuth-Morris-Pratt) Pattern Matching Algorithm in Java
// Time Complexity: O(n + m), where n = length of text, m = length of pattern
// Space Complexity: O(m)

public class KMPPatternMatching {

    // Builds the LPS (Longest Prefix Suffix) array
    private static int[] buildLPS(String pattern) {
        int m = pattern.length();
        int[] lps = new int[m];
        int len = 0; // length of previous longest prefix suffix
        int i = 1;

        while (i < m) {
            if (pattern.charAt(i) == pattern.charAt(len)) {
                len++;
                lps[i] = len;
                i++;
            } else {
                if (len != 0) {
                    len = lps[len - 1];
                } else {
                    lps[i] = 0;
                    i++;
                }
            }
        }
        return lps;
    }

    // KMP search function
    public static void KMPSearch(String text, String pattern) {
        int n = text.length();
        int m = pattern.length();

        int[] lps = buildLPS(pattern);
        int i = 0, j = 0; // i -> text, j -> pattern

        while (i < n) {
            if (text.charAt(i) == pattern.charAt(j)) {
                i++;
                j++;
            }

            if (j == m) {
                System.out.println("Pattern found at index: " + (i - j));
                j = lps[j - 1]; // continue searching
            } else if (i < n && text.charAt(i) != pattern.charAt(j)) {
                if (j != 0)
                    j = lps[j - 1];
                else
                    i++;
            }
        }
    }

    // Example usage
    public static void main(String[] args) {
        String text = "ABABDABACDABABCABAB";
        String pattern = "ABABCABAB";

        KMPSearch(text, pattern);
    }
}

