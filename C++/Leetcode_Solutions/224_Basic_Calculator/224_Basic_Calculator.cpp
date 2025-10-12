#include <bits/stdc++.h>
using namespace std;

/**
 * @class Solution
 * @brief Evaluates a basic mathematical expression string containing '+', '-', and parentheses.
 *
 * The expression can include positive integers, addition, subtraction,
 * and nested parentheses. Spaces are ignored.
 *
 * Example:
 * Input: "(1+(4+5+2)-3)+(6+8)"
 * Output: 23
 */
class Solution {
public:
    /**
     * @brief Calculates the result of the given arithmetic expression.
     * 
     * @param expression The input string containing numbers, '+', '-', '(', ')'.
     * @return The computed integer result.
     *
     * Logic:
     * - Use a stack to handle nested expressions inside parentheses.
     * - Keep track of:
     *     - `result`: current accumulated value.
     *     - `sign`: current sign (+1 or -1).
     *     - `number`: current number being formed.
     * - When '(' is encountered, push current result and sign onto stack and reset them.
     * - When ')' is encountered, pop sign and previous result to compute nested result.
     */
    int calculate(string expression) {
        stack<int> operationsStack;  // Stack to store previous results and signs
        int sign = 1;                // Current sign (+1 or -1)
        int number = 0;              // Current number being processed
        int result = 0;              // Current accumulated result

        for (char ch : expression) {
            if (isdigit(ch)) {
                // Build the current number (handles multi-digit numbers)
                number = (number * 10) + (ch - '0');
            } 
            else if (ch == '+' || ch == '-') {
                // Apply the previous number with its sign to result
                result += number * sign;
                // Update the sign for the next number
                sign = (ch == '+') ? 1 : -1;
                number = 0;  // Reset number
            } 
            else if (ch == '(') {
                // Push current result and sign onto the stack
                operationsStack.push(result);
                operationsStack.push(sign);
                // Reset for new expression inside parentheses
                result = 0;
                sign = 1;
            } 
            else if (ch == ')') {
                // Complete current number before closing bracket
                result += sign * number;
                number = 0;
                // Multiply by sign before parentheses
                result *= operationsStack.top(); 
                operationsStack.pop();
                // Add the result before parentheses
                result += operationsStack.top(); 
                operationsStack.pop();
            }
            // Ignore spaces or other characters
        }

        // Add the last number if any remains
        return result + number * sign;
    }
};

/**
 * @brief Example usage and test cases.
 */
int main() {
    Solution solver;

    cout << solver.calculate("1 + 1") << endl;                 // Output: 2
    cout << solver.calculate(" 2-1 + 2 ") << endl;             // Output: 3
    cout << solver.calculate("(1+(4+5+2)-3)+(6+8)") << endl;   // Output: 23
    cout << solver.calculate("-(3+(2-1))") << endl;            // Output: -4

    return 0;
}

/**
 * @complexity
 * Time Complexity: O(n)
 *
 * Space Complexity: O(n)
 */
