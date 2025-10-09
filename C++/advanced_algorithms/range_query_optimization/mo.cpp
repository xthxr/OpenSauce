/*
    Problem Statement :-
    -----------------
    Given an array and multiple queries [L, R], find the sum of elements in each query range efficiently using Mo’s Algorithm.

    Algorithm :-
    -----------
    1. Mo’s Algorithm is an offline query optimization technique.
    2. Sort all queries in a specific order: by block (L / sqrt(N)), then by R.
    3. Maintain a current range [currL, currR] and current sum.
    4. Move L and R incrementally to match each query range, updating the sum.
    5. This reduces complexity from O(Q * N) → O((N + Q) * sqrt(N)).

    Example :-
    ----------
    Input :-
    5 3
    1 2 3 4 5
    0 2
    1 3
    2 4

    Output :-
    6
    9
    12
*/

#include <bits/stdc++.h>
using namespace std;

struct Query {
    int L, R, idx;
};

int BLOCK;

bool compare(Query a, Query b) {
    if (a.L / BLOCK != b.L / BLOCK)
        return a.L / BLOCK < b.L / BLOCK;
    return a.R < b.R;
}

int main() {
    #ifndef ONLINE_JUDGE
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    #endif

    int n, q;
    cin >> n >> q;
    vector<int> arr(n);
    for (int i = 0; i < n; i++) cin >> arr[i];

    vector<Query> queries(q);
    for (int i = 0; i < q; i++) {
        cin >> queries[i].L >> queries[i].R;
        queries[i].idx = i;
    }

    BLOCK = sqrt(n);
    sort(queries.begin(), queries.end(), compare);

    vector<int> ans(q);
    int currL = 0, currR = -1, currSum = 0;

    for (auto &query : queries) {
        int L = query.L, R = query.R;
        while (currR < R) currSum += arr[++currR];
        while (currR > R) currSum -= arr[currR--];
        while (currL < L) currSum -= arr[currL++];
        while (currL > L) currSum += arr[--currL];
        ans[query.idx] = currSum;
    }

    for (auto &x : ans) cout << x << "\n";
    return 0;
}
