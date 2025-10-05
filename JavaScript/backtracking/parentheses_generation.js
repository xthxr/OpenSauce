/*
Problem: Generate Parentheses
Topic: Backtracking
Description: Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.
Time Complexity: O(2^2n) or O(n)
Space Complexity: O(n)
*/

// Function to generate all valid combinations of n pairs of parentheses
var generateParenthesis = function(n) {
    const res = []; // to store all valid combinations

    // dfs (Depth-First Search) function for backtracking
    function dfs(openP, closeP, s) {
        // Base case: if we used all parentheses (n pairs)
        if (openP === n && closeP === n) {
            res.push(s); // add the valid combination to result
            return;
        }

        // If we can still add an open parenthesis
        if (openP < n) {
            dfs(openP + 1, closeP, s + "(");
        }

        // If we can add a closing parenthesis (only if it won’t break balance)
        if (closeP < openP) {
            dfs(openP, closeP + 1, s + ")");
        }
    }

    // Start the recursion with 0 open, 0 close, and empty string
    dfs(0, 0, "");

    return res;
};

// Example usage
console.log(generateParenthesis(3));
// Output: ["((()))", "(()())", "(())()", "()(())", "()()()"]