/*
    Problem Statement :-
    -----------------
    Given a text and a pattern, find all occurrences of the pattern in the text using the KMP (Knuth–Morris–Pratt) Algorithm.

    Algorithm :-
    -----------
    1. Preprocess pattern to build LPS (Longest Prefix Suffix) array.
    2. Use it to skip unnecessary re-comparisons in the main search.
    3. LPS[i] = longest proper prefix of pat[0..i] which is also a suffix.
    4. Overall time complexity: O(n + m).

    Example :-
    ----------
    Input :-
    text: ABABDABACDABABCABAB
    pattern: ABABCABAB
    Output :-
    Pattern found at index: 10
*/

#include <bits/stdc++.h>
using namespace std;

vector<int> computeLPS(string pat) {
    int m = pat.size();
    vector<int> lps(m, 0);
    int len = 0;

    for (int i = 1; i < m; ) {
        if (pat[i] == pat[len]) lps[i++] = ++len;
        else if (len != 0) len = lps[len - 1];
        else lps[i++] = 0;
    }
    return lps;
}

int main() {
    #ifndef ONLINE_JUDGE
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    #endif

    string txt, pat;
    cin >> txt >> pat;

    int n = txt.size(), m = pat.size();
    vector<int> lps = computeLPS(pat);

    int i = 0, j = 0;
    while (i < n) {
        if (pat[j] == txt[i]) i++, j++;
        if (j == m) {
            cout << "Pattern found at index: " << i - j << "\n";
            j = lps[j - 1];
        } else if (i < n && pat[j] != txt[i]) {
            if (j != 0) j = lps[j - 1];
            else i++;
        }
    }
    return 0;
}
