/*
Problem:Longest Substring Without Repeating Characters
Topic: string
Description: Given a string s, find the length of the longest substring without duplicate characters.
Time Complexity: O(n)
Space Complexity: O(1)
*/

#include <stdio.h>
#include <string.h>
#include <limits.h>

int lengthOfLongestSubstring(char *s) {
    int n = strlen(s);
    if (n == 0) return 0;

    int freq[256] = {0};  // For all ASCII characters
    int i = 0, j = 0, ans = 0;

    while (j < n) {
        freq[(unsigned char)s[j]]++;

        // If there’s a duplicate
        while (freq[(unsigned char)s[j]] > 1) {
            freq[(unsigned char)s[i]]--;
            i++;
        }

        // Update max length
        if (j - i + 1 > ans)
            ans = j - i + 1;

        j++;
    }

    return ans;
}

int main() {
    char s[100];
    printf("Enter a string: ");
    scanf("%99s", s);

    int result = lengthOfLongestSubstring(s);
    printf("Length of the longest substring without repeating characters: %d\n", result);

    return 0;
}

// Explanation:
// freq[256]: acts like your unordered_map — counts occurrences of each ASCII char.
// i and j: represent the sliding window.
// If freq[s[j]] > 1, we shrink the window from the left (i++) to remove duplicates.
// Keep track of the maximum window size with ans.