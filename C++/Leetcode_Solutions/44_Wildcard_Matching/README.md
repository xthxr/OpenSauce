# Wildcard Matching (LeetCode 44)

## Problem Statement
Given a string `s` and a pattern `p` which can include the special characters:
- '?' matches any single character.
- '*' matches any sequence of characters (including the empty sequence).

Implement a function that returns `true` if `s` matches the pattern `p` and `false` otherwise.

### Example 1
Input:  
s = "adceb"
p = "*a*b"
Output:  
true
Explanation:  
'*' matches any sequence (here "ad"), '?' matches a single character, and the rest of the pattern matches the string.

### Example 2
Input:  
s = "acdcb"
p = "a*c?b"
Output:  
false

---

## Intuition
- Brute force recursion is too slow due to overlapping subproblems.  
- Use **Dynamic Programming (DP)** to store intermediate results.  
- Two approaches:
  1. **Top-Down DP (Memoization)**  
  2. **Bottom-Up DP (Tabulation)**  

---

## Approach

### 1️⃣ Top-Down (Memoization)
- Define recursive function `f(i, j)` checking if `s[i:]` matches `p[j:]`.
- **Base Case:** If end of `s` is reached, check if remaining pattern contains only '*'.
- **Choices:**
  - If `s[i] == p[j]` or `p[j] == '?'` → move both pointers.
  - If `p[j] == '*'` → try three options:
    1. Match zero characters: move pattern pointer (`j + 1`)  
    2. Match one character: move both pointers (`i + 1, j + 1`)  
    3. Match multiple characters: move string pointer (`i + 1`)  
- Store results in `dp[i][j]` to avoid recomputation.

### 2️⃣ Bottom-Up (Tabulation)
- Create 2D boolean DP table `dp[n+1][m+1]` where `dp[i][j] = true` if `s[0..i-1]` matches `p[0..j-1]`.
- **Initialization:**
  - `dp[0][0] = true` (empty string matches empty pattern)  
  - `dp[0][j] = dp[0][j-1] && p[j-1]=='*'` (empty string matches pattern only if pattern is all '*')
- **Filling the table:**
  - If characters match or pattern is '?' → `dp[i][j] = dp[i-1][j-1]`
  - If pattern is '*' → `dp[i][j] = dp[i-1][j] || dp[i][j-1] || dp[i-1][j-1]`  

---

## Step-by-Step Example

**Input:**  
s = "adceb"
p = "*a*b"

**DP Table (Bottom-Up):**

| i\j | '' | * | a | * | b |
|-----|----|---|---|---|---|
| ''  | T  | T | F | F | F |
| a   | F  | T | T | T | F |
| d   | F  | T | F | T | F |
| c   | F  | T | F | T | F |
| e   | F  | T | F | T | F |
| b   | F  | T | F | F | T |

Final Result: `dp[n][m] = true` ✅

---

## Complexity Analysis

| Approach | Time Complexity | Space Complexity |
|----------|----------------|----------------|
| Top-Down DP | O(n * m) | O(n * m) + recursion stack |
| Bottom-Up DP | O(n * m) | O(n * m) |

Where `n = s.length()` and `m = p.length()`.

---

## File Organization

```
dynamic_programming/
├── bottom_up_wildcard_matching.cpp
├── top_down_wildcard_matching.cpp
└── README.md
```

---

## Example Usage (C++)

```cpp
#include <bits/stdc++.h>
using namespace std;

int main() {
    Solution sol;
    string s = "adceb";
    string p = "*a*b";
    cout << (sol.isMatch(s, p) ? "Match" : "No Match") << endl;
    return 0;
}
```

**Output:**
```
Match
```
