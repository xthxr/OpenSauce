//Problem: A phrase is a palindrome if, after converting all uppercase letters into lowercase letters and removing all non-alphanumeric characters, it reads the same forward and backward. Alphanumeric characters include letters and numbers.
// Given a string s, return true if it is a palindrome, or false otherwise.

//Problem link: https://leetcode.com/problems/valid-palindrome/?envType=problem-list-v2&envId=string

//Solution Code:

function isPalindrome(s) {
    // Remove non-alphanumeric characters and convert to lowercase
    const cleaned = s.replace(/[^a-z0-9]/gi, '').toLowerCase();

    // Compare characters from start and end
    let left = 0;
    let right = cleaned.length - 1;

    while (left < right) {
        if (cleaned[left] !== cleaned[right]) {
            return false;
        }
        left++;
        right--;
    }
    return true;
}

// Example usage
console.log(isPalindrome("A man, a plan, a canal: Panama")); // true
console.log(isPalindrome("race a car"));                     // false
console.log(isPalindrome(" "));                              // true
