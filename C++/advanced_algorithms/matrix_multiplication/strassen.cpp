/*
    Problem Statement :-
    -----------------
    Implement Strassen's Algorithm for matrix multiplication and compare it with the standard O(n^3) method.

    Algorithm :-
    -----------
    1. Strassen’s Algorithm is a divide and conquer algorithm that reduces the number of multiplications
       needed for matrix multiplication from 8 to 7 per recursion level.
    2. It works on square matrices of size 2^n x 2^n.
    3. Divide the matrices A and B into 4 submatrices each of size n/2.
    4. Compute 7 products (M1 to M7) recursively using combinations of submatrices.
    5. Combine results to get the final matrix.

    Time Complexity :- O(n^2.81)
    Space Complexity :- O(n^2)

    Example :-
    ----------
    Input :-
    2
    1 2
    3 4
    5 6
    7 8

    Output :-
    Resultant Matrix (Strassen):
    19 22
    43 50
*/

#include <bits/stdc++.h>
using namespace std;

vector<vector<int>> add(const vector<vector<int>>& A, const vector<vector<int>>& B) {
    int n = A.size();
    vector<vector<int>> C(n, vector<int>(n));
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
            C[i][j] = A[i][j] + B[i][j];
    return C;
}

vector<vector<int>> sub(const vector<vector<int>>& A, const vector<vector<int>>& B) {
    int n = A.size();
    vector<vector<int>> C(n, vector<int>(n));
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++)
            C[i][j] = A[i][j] - B[i][j];
    return C;
}

vector<vector<int>> strassen(const vector<vector<int>>& A, const vector<vector<int>>& B) {
    int n = A.size();
    if (n == 1) return {{A[0][0] * B[0][0]}};

    int k = n / 2;
    vector<vector<int>> A11(k, vector<int>(k)), A12(k, vector<int>(k)),
                        A21(k, vector<int>(k)), A22(k, vector<int>(k)),
                        B11(k, vector<int>(k)), B12(k, vector<int>(k)),
                        B21(k, vector<int>(k)), B22(k, vector<int>(k));

    for (int i = 0; i < k; i++)
        for (int j = 0; j < k; j++) {
            A11[i][j] = A[i][j];
            A12[i][j] = A[i][j + k];
            A21[i][j] = A[i + k][j];
            A22[i][j] = A[i + k][j + k];
            B11[i][j] = B[i][j];
            B12[i][j] = B[i][j + k];
            B21[i][j] = B[i + k][j];
            B22[i][j] = B[i + k][j + k];
        }

    auto M1 = strassen(add(A11, A22), add(B11, B22));
    auto M2 = strassen(add(A21, A22), B11);
    auto M3 = strassen(A11, sub(B12, B22));
    auto M4 = strassen(A22, sub(B21, B11));
    auto M5 = strassen(add(A11, A12), B22);
    auto M6 = strassen(sub(A21, A11), add(B11, B12));
    auto M7 = strassen(sub(A12, A22), add(B21, B22));

    auto C11 = add(sub(add(M1, M4), M5), M7);
    auto C12 = add(M3, M5);
    auto C21 = add(M2, M4);
    auto C22 = add(sub(add(M1, M3), M2), M6);

    vector<vector<int>> C(n, vector<int>(n));
    for (int i = 0; i < k; i++)
        for (int j = 0; j < k; j++) {
            C[i][j] = C11[i][j];
            C[i][j + k] = C12[i][j];
            C[i + k][j] = C21[i][j];
            C[i + k][j + k] = C22[i][j];
        }
    return C;
}

int main() {
    #ifndef ONLINE_JUDGE
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    #endif

    int n;
    cin >> n;
    vector<vector<int>> A(n, vector<int>(n)), B(n, vector<int>(n));

    for (auto &row : A)
        for (auto &x : row) cin >> x;
    for (auto &row : B)
        for (auto &x : row) cin >> x;

    auto C = strassen(A, B);

    cout << "Resultant Matrix (Strassen):\n";
    for (auto &row : C) {
        for (auto &x : row) cout << x << " ";
        cout << "\n";
    }
    return 0;
}
