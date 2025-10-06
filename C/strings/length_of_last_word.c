/*
Problem:Length of the last word
Topic: string
Description: Given a string s consisting of words and spaces, return the length of the last word in the string.
A word is a maximal substring consisting of non-space characters only.
Time Complexity: O(n)
Space Complexity: O(1)
*/

#include <stdio.h>
#include <string.h>

// Function to find length of last word in a string
int lengthOfLastWord(char* s) {
    int n = strlen(s);  // Get length of the string
    int len = 0;        // To store length of the last word
    int found = 0;      // Flag to indicate we've started counting

    // Traverse the string from the end
    for (int i = n - 1; i >= 0; i--) {
        if (s[i] != ' ') {
            len++;     // Increment length if not a space
            found = 1; // Mark that we started counting a word
        } else if (found) {
            // If we hit a space after counting, last word ended
            break;
        }
    }

    return len;
}

// Driver code
int main() {
    char str[] = "Hello World";
    printf("Length of last word: %d\n", lengthOfLastWord(str));
    return 0;
}
