/*
    Problem Statement :- You are given a 2D grid `heights` where heights[i][j] represents the height of a cell.
    Find a path from the top-left cell (0,0) to the bottom-right cell (n-1,m-1) such that 
    the **maximum absolute difference in heights** between consecutive cells along the path 
    is minimized. Return the minimum effort required to travel from start to end.

    ------------
    
    Algorithm :- (Using Dijkstra’s Algorithm)
    ------------
    1. We model the grid as a graph where each cell is a node, and edges exist between adjacent cells.
       The "cost" to move from one cell to another is the absolute height difference between them.
    2. Initialize a 2D `effortArr` to store the minimum effort required to reach each cell. 
       Set all values to INT_MAX initially.
    3. Use a **min-heap priority queue** (`pq`) where each element is of the form:
       {current_effort, {row, col}}.
    4. Start from (0,0) with effort = 0, and push it into the queue.
    5. While the priority queue is not empty:
        a. Pop the cell with the smallest effort.
        b. If this cell is the destination (bottom-right), return the effort.
        c. Traverse all 4 directions (up, down, left, right):
            - For each valid neighboring cell, calculate:
              newEffort = max(current_effort, abs(height difference))
            - If `newEffort` is smaller than the current stored effort for that cell,
              update it and push it into the priority queue.
    6. If the destination is never reached, return 0 (though logically, it’s always reachable).

    Example :-
    ------------
    Input :-
    ------------
    heights = [
      [1,2,2],
      [3,8,2],
      [5,3,5]
    ]

    Output :-
    ------------
    2

    Explanation :-
    ------------
    The path [ (0,0) -> (0,1) -> (0,2) -> (1,2) -> (2,2) ] 
    has the minimum maximum effort = 2.
*/


#include<bits/stdc++.h>
using namespace std;

typedef pair<int, pair<int, int>> pii;

class Solution {
public:
    int minimumEffortPath(vector<vector<int>>& heights) {

        int rowSize = heights.size(); 
        int colSize = heights[0].size(); 
        
        vector<vector<int>> effortArr(rowSize, vector<int>(colSize, INT_MAX));
        
        priority_queue<pii, vector<pii>, greater<pii>> pq;
        
        pq.push({0, {0,0}});
        effortArr[0][0]=0;

        int dR[] = {0, 0, 1, -1};
        int dC[] = {1, -1, 0, 0};

        while(!pq.empty()){
            pii it = pq.top();
            int row = it.second.first;
            int col = it.second.second;
            int effort = it.first;
            pq.pop();

            if(row == rowSize-1 && col == colSize-1) return effort;
            
            for(int i=0;i<4;i++){
                int nRow = row + dR[i];
                int nCol = col + dC[i];

                if(nRow >= 0 && nRow < rowSize && nCol >= 0 && nCol < colSize){

                    int newEffort = max(abs(heights[row][col] - heights[nRow][nCol]), effort);

                    if(effortArr[nRow][nCol] > newEffort){
                        
                        effortArr[nRow][nCol] = newEffort;
                        pq.push({newEffort, {nRow, nCol}});

                    }
                }
            }
            
        }

        return 0;
    }
};
