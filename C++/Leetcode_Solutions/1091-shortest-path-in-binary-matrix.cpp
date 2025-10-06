/*
    Problem Statement :- Given an n x n binary matrix grid, find the shortest clear path 
    from the top-left cell (0,0) to the bottom-right cell (n-1,n-1). 
    A clear path can only pass through cells with value 0, and you can move in 8 possible directions.
    Return the length of the shortest path. If no path exists, return -1.
    ------------

    Algorithm :-
    ------------
    1. If the starting cell (0,0) is blocked, return -1 immediately.
    2. Initialize a distance matrix `dist` of size n x n with INT_MAX to store the minimum distance to each cell.
    3. Use a queue for BFS, since BFS guarantees the shortest path in an unweighted grid.
    4. Start from cell (0,0), mark its distance as 1, and push it into the queue.
    5. Define movement in 8 directions using two arrays `row[]` and `col[]` to represent all possible moves.
    6. While the queue is not empty:
        a. Pop the front cell (x, y).
        b. Traverse all 8 possible directions.
        c. For each valid and unblocked neighboring cell (nRow, nCol):
            - If a shorter path is found, update `dist[nRow][nCol] = dist[x][y] + 1`
            - Push the cell into the queue.
    7. After BFS completes, check if bottom-right cell (n-1, n-1) was reached.
       - If yes, return its distance.
       - Otherwise, return -1 (path doesn’t exist).

    Example :-
    ------------
    Input :-
    ------------
    grid = [
      [0,1],
      [1,0]
    ]

    Output :-
    ------------
    2

    Explanation :-
    ------------
    The shortest clear path is [ (0,0) -> (1,1) ], which has length 2.
*/


#include<bits/stdc++.h>
using namespace std;

typedef pair<int, int> pii;

class Solution {
public:
    int shortestPathBinaryMatrix(vector<vector<int>>& grid) {
        int n = grid.size(); 
        if(grid[0][0] != 0) return -1;

        vector<vector<int>> dist(n, vector<int>(n, INT_MAX));
        queue<pii> q;
        
        q.push({0,0});
        dist[0][0]=1;

        int row[] = {0, 0, 1, -1, 1, 1, -1, -1};
        int col[] = {1, -1, 0, 0, 1, -1, 1, -1};

        while(!q.empty()){
            auto it = q.front();
            int x = it.first;
            int y = it.second;

            q.pop();
            
            for(int i=0;i<8;i++){
                int nRow = x+row[i];
                int nCol = y+col[i];
                if(nRow >= 0 && nRow < n && nCol >= 0 && nCol < n && grid[nRow][nCol] == 0){
                    int newDist = dist[x][y]+1;
                    if(dist[nRow][nCol] > newDist){
                        dist[nRow][nCol] = newDist;
                        q.push({nRow, nCol});
                    }
                }
            }
        }

        return dist[n-1][n-1] == INT_MAX ? -1 : dist[n-1][n-1];
    }
};
