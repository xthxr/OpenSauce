/*
    Problem Statement :-
    -----------------
    Given a text and a pattern, find all occurrences of the pattern in the text using the Rabin–Karp Algorithm.

    Algorithm :-
    -----------
    1. Compute hash of pattern (p) and first window of text (t).
    2. Slide the window by 1, update hash in O(1) using rolling hash.
    3. If hashes match, compare substring to verify (avoid false positives).
    4. Uses modular arithmetic to prevent overflow.

    Example :-
    ----------
    Input :-
    text: ABCCDDAEFG
    pattern: CCD
    Output :-
    Pattern found at index: 2
*/

#include <bits/stdc++.h>
using namespace std;
#define d 256
#define q 101  // A prime number for modulo hashing

int main() {
    #ifndef ONLINE_JUDGE
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    #endif

    string txt, pat;
    cin >> txt >> pat;

    int n = txt.size(), m = pat.size();
    int p = 0, t = 0, h = 1;

    for (int i = 0; i < m - 1; i++)
        h = (h * d) % q;

    for (int i = 0; i < m; i++) {
        p = (d * p + pat[i]) % q;
        t = (d * t + txt[i]) % q;
    }

    for (int i = 0; i <= n - m; i++) {
        if (p == t) {
            bool match = true;
            for (int j = 0; j < m; j++) {
                if (txt[i + j] != pat[j]) {
                    match = false; break;
                }
            }
            if (match) cout << "Pattern found at index: " << i << "\n";
        }

        if (i < n - m) {
            t = (d * (t - txt[i] * h) + txt[i + m]) % q;
            if (t < 0) t += q;
        }
    }
    return 0;
}
