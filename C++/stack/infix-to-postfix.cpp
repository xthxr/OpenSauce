/*
    Problem Statement :- Convert an infix expression (e.g., A + B * C) 
    into its equivalent postfix expression (e.g., A B C * +), 
    following operator precedence and associativity rules.

    --------------
    
    Algorithm :- (Using Stack for Operator Precedence)
    --------------
    1. Initialize an empty stack `st` for operators and a string `ans` for the postfix output.
    2. Traverse the given infix expression character by character:
        a. If the character is an **operand** (A–Z, a–z, 0–9), append it to `ans`.
        b. If it’s an **opening bracket '('**, push it onto the stack.
        c. If it’s a **closing bracket ')'**, pop from the stack and append to `ans`
           until an opening bracket '(' is encountered. Then remove '('.
        d. If it’s an **operator (+, -, *, /, ^)**:
            - If the stack is empty or has a lower precedence operator, push it.
            - Otherwise, pop operators from the stack and append to `ans` while:
                • The top of the stack is not '('  
                • And precedence(top) >= precedence(current)  
                • And (if current is '^', treat it as **right-associative**)  
              Then push the current operator.
    3. After traversing all characters, pop and append any remaining operators from the stack to `ans`.
    4. Return `ans` as the postfix expression.

    --------------
    
    Operator Precedence :-
    --------------
    ^  → highest (right-associative)  
    * / → next (left-associative)  
    + - → lowest (left-associative)

    --------------
    
    Example :-
    --------------
    Input :-
    --------------
    s = "(A+B)*C-D"
    
    Output :-
    --------------
    "AB+C*D-"
    
    Explanation :-
    --------------
    The infix expression (A+B)*C-D converts to postfix as:  
    (A+B) → AB+  
    (AB+)*C → AB+C*  
    AB+C*-D → AB+C*D-
*/

#include <bits/stdc++.h>
using namespace std;

class Solution {
  public:
    
    bool isOperand(char &c){
        return (c >= 'A' && c <= 'Z') || (c >= 'a' && c<= 'z') || (c >= '0' && c<= '9');
    }
            
    int precedence(char &c){
        switch(c){
            case '^': return 3;
            case '*': case '/': return 2;
            case '+': case '-': return 1;
            default: return 0;
        }
    };
    
    string infixToPostfix(string& s) {
        stack<char> st;
        string ans = "";

        for(char c: s){
            if(isOperand(c)){
                ans+=c;
            }
            else if (c == '('){
                st.push(c);
            }
            else if(c == ')') {
                while(!st.empty() && st.top() != '('){
                    ans+=st.top();
                    st.pop();
                }
                st.pop();
            }
            else{
                if(!st.empty() && precedence(c) > precedence(st.top())){
                    st.push(c);
                }
                else {
                    // special condition handled for ^ operator since it's right-associative
                    // pop when: prec(top) > prec(c), or when: prec(top) == prec(c) && c != '^'
                    while(!st.empty() && st.top() != '(' && precedence(st.top()) >= precedence(c) && c != '^'){
                        ans+=st.top();
                        st.pop();
                    }
                    st.push(c);
                }
            }
        }
        while(!st.empty()){
            ans+=st.top();
            st.pop();
        }
        return ans;
    }
};
