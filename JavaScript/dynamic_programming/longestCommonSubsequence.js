/**
 * Problem: Longest Common Subsequence (LCS)
 *
 * Given two strings, find the length of the longest subsequence present in both of them.
 * A subsequence is a sequence that appears in the same relative order, but not necessarily contiguous.
 * For example, "ace" is a subsequence of "abcde".
 *
 * This problem can be solved efficiently using a 2D dynamic programming table.
 */

/**
 * Finds the length of the longest common subsequence between two strings.
 * @param {string} text1 The first string.
 * @param {string} text2 The second string.
 * @returns {number} The length of the LCS.
 */
const longestCommonSubsequence = (text1, text2) => {
  const m = text1.length;
  const n = text2.length;

  // Create a 2D DP table, initialized with 0s.
  // dp[i][j] will store the length of the LCS of text1[0..i-1] and text2[0..j-1].
  // The table size is (m+1) x (n+1) to handle base cases (empty strings).
  const dp = Array(m + 1).fill(null).map(() => Array(n + 1).fill(0));

  // Fill the DP table from top-left to bottom-right.
  for (let i = 1; i <= m; i++) {
    for (let j = 1; j <= n; j++) {
      // Get the characters from the original strings.
      // Note the i-1 and j-1 because strings are 0-indexed while our table is 1-indexed.
      const char1 = text1[i - 1];
      const char2 = text2[j - 1];

      // Case 1: The characters match.
      if (char1 === char2) {
        // If they match, the LCS length is 1 + the LCS of the strings without these characters.
        // We look at the diagonally up-left cell in our table.
        dp[i][j] = 1 + dp[i - 1][j - 1];
      } else {
        // Case 2: The characters do not match.
        // The LCS will be the maximum of the LCS found by either:
        // a) Ignoring the current character of text1 (looking at the cell above).
        // b) Ignoring the current character of text2 (looking at the cell to the left).
        dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
      }
    }
  }

  // The bottom-right cell of the table contains the length of the LCS for the full strings.
  return dp[m][n];
};


// --- Example Usage ---

console.log("--- Longest Common Subsequence Problem ---");

const textA = "abcde";
const textB = "ace";
const lcsLength1 = longestCommonSubsequence(textA, textB);
console.log(`Text 1: "${textA}"`);
console.log(`Text 2: "${textB}"`);
console.log(`Length of LCS: ${lcsLength1}`); // Expected output: 3 ("ace")
console.log("-" * 20);


const textC = "AGGTAB";
const textD = "GXTXAYB";
const lcsLength2 = longestCommonSubsequence(textC, textD);
console.log(`Text 1: "${textC}"`);
console.log(`Text 2: "${textD}"`);
console.log(`Length of LCS: ${lcsLength2}`); // Expected output: 4 ("GTAB")
console.log("-" * 20);

const textE = "abc";
const textF = "def";
const lcsLength3 = longestCommonSubsequence(textE, textF);
console.log(`Text 1: "${textE}"`);
console.log(`Text 2: "${textF}"`);
console.log(`Length of LCS: ${lcsLength3}`); // Expected output: 0
