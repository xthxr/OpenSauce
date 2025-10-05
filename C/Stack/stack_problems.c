#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX 100

/*
Title: Combined Stack Problem Solutions in C
Description: This file contains solutions to 5 common stack problems:
1. Reverse a String
2. Balanced Parentheses
3. Evaluate Postfix Expression
4. Next Greater Element
5. Sort a Stack
Each problem includes time complexity analysis and uses stack data structure concepts.
*/

// =========================
// Problem 1: Reverse a String
// Time Complexity: O(n) for pushing and popping each character
// Space Complexity: O(n) for the stack
// =========================
void reverseString() {
    char str[MAX];
    printf("\n--- Problem 1: Reverse a String ---\n");
    printf("Enter string: ");
    scanf("%s", str);

    char stack[MAX];
    int top = -1;

    for(int i = 0; str[i]; i++) stack[++top] = str[i]; // push
    for(int i = 0; str[i]; i++) str[i] = stack[top--]; // pop

    printf("Reversed string: %s\n", str);
}

// =========================
// Problem 2: Balanced Parentheses
// Time Complexity: O(n)
// Space Complexity: O(n) for stack
// =========================
void balancedParentheses() {
    char expr[MAX];
    printf("\n--- Problem 2: Balanced Parentheses ---\n");
    printf("Enter expression: ");
    scanf("%s", expr);

    char stack[MAX];
    int top = -1;
    int balanced = 1;

    for(int i = 0; expr[i]; i++) {
        char ch = expr[i];
        if(ch == '(' || ch == '{' || ch == '[') stack[++top] = ch;
        else if(ch == ')' || ch == '}' || ch == ']') {
            if(top == -1) { balanced = 0; break; }
            char last = stack[top--];
            if((ch == ')' && last != '(') ||
               (ch == '}' && last != '{') ||
               (ch == ']' && last != '[')) { balanced = 0; break; }
        }
    }
    if(top != -1) balanced = 0;

    printf("%s\n", balanced ? "Balanced" : "Not Balanced");
}

// =========================
// Problem 3: Evaluate Postfix Expression
// Time Complexity: O(n)
// Space Complexity: O(n) for stack
// =========================
void postfixEvaluation() {
    char expr[MAX];
    printf("\n--- Problem 3: Evaluate Postfix Expression ---\n");
    printf("Enter postfix expression (digits only, e.g., 231*+9-): ");
    scanf("%s", expr);

    int stack[MAX], top = -1;

    for(int i = 0; expr[i]; i++) {
        char ch = expr[i];
        if(isdigit(ch)) stack[++top] = ch - '0';
        else {
            int val2 = stack[top--];
            int val1 = stack[top--];
            switch(ch) {
                case '+': stack[++top] = val1 + val2; break;
                case '-': stack[++top] = val1 - val2; break;
                case '*': stack[++top] = val1 * val2; break;
                case '/': stack[++top] = val1 / val2; break;
            }
        }
    }
    printf("Result: %d\n", stack[top]);
}

// =========================
// Problem 4: Next Greater Element
// Time Complexity: O(n)
// Space Complexity: O(n) for stack and result array
// =========================
void nextGreaterElement() {
    printf("\n--- Problem 4: Next Greater Element ---\n");
    int n;
    printf("Enter number of elements: ");
    scanf("%d", &n);
    int arr[MAX];
    printf("Enter elements: ");
    for(int i = 0; i < n; i++) scanf("%d", &arr[i]);

    int stack[MAX], top = -1;
    int nge[MAX];

    for(int i = n-1; i >= 0; i--) {
        while(top != -1 && stack[top] <= arr[i]) top--;
        nge[i] = (top == -1) ? -1 : stack[top];
        stack[++top] = arr[i];
    }

    printf("Next Greater Elements: ");
    for(int i = 0; i < n; i++) printf("%d ", nge[i]);
    printf("\n");
}

// =========================
// Problem 5: Sort a Stack
// Time Complexity: O(n^2) in worst case
// Space Complexity: O(n) for recursion stack
// =========================
void sortStack() {
    printf("\n--- Problem 5: Sort a Stack ---\n");
    int stack[MAX], top = -1;
    int n, x;

    printf("Enter number of elements: ");
    scanf("%d", &n);

    printf("Enter elements: ");
    for(int i = 0; i < n; i++) { scanf("%d", &x); stack[++top] = x; }

    // Recursive helper
    void sortedInsert(int val) {
        if(top == -1 || val > stack[top]) stack[++top] = val;
        else {
            int temp = stack[top--];
            sortedInsert(val);
            stack[++top] = temp;
        }
    }

    void sort() {
        if(top != -1) {
            int temp = stack[top--];
            sort();
            sortedInsert(temp);
        }
    }

    printf("Original Stack: ");
    for(int i = 0; i <= top; i++) printf("%d ", stack[i]);
    printf("\n");

    sort();

    printf("Sorted Stack: ");
    for(int i = 0; i <= top; i++) printf("%d ", stack[i]);
    printf("\n");
}

// =========================
// Main
// =========================
int main() {
    reverseString();
    balancedParentheses();
    postfixEvaluation();
    nextGreaterElement();
    sortStack();
    return 0;
}
