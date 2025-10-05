# LeetCode Question  934. Shortest Bridge

"""
You are given an n x n binary matrix grid where 1 represents land and 0 represents water.
An island is a 4-directionally connected group of 1's not connected to any other 1's. There are exactly two islands in grid.
You may change 0's to 1's to connect the two islands to form one island.
Return the smallest number of 0's you must flip to connect the two islands.


- Example 1:
    Input: grid = [[0,1],[1,0]]
    Output: 1

- Example 2:
    Input: grid = [[0,1,0],[0,0,0],[0,0,1]]
    Output: 2

- Example 3:
    Input: grid = [[1,1,1,1,1],[1,0,0,0,1],[1,0,1,0,1],[1,0,0,0,1],[1,1,1,1,1]]
    Output: 1


Constraints:

    n == grid.length == grid[i].length
    2 <= n <= 100
    grid[i][j] is either 0 or 1.
    There are exactly two islands in grid.

"""

# SOLUTION

from collections import deque
from typing import List


class Solution:
    def shortestBridge(self, grid: List[List[int]]) -> int:
        """
        Return the fewest water cells (0 → 1 flips) needed to connect the
        two islands in an n × n binary grid.

        Algorithm overview
        ------------------
        1) **Locate & mark one island (DFS)**  
        – Paint every land-cell (1) of the first island to 2  
        – Push each painted cell into a queue

        2) **Expand level-by-level from that island (BFS)**  
        – Treat the queue as the current “wave-front”  
        – Each BFS layer flips one more ring of water to land  
        – The first time we touch the second island (a 1), the
            number of layers popped so far equals the answer

        Why this is DP/BFS?  
        • The DFS step is pure graph traversal.  
        • The BFS step performs dynamic programming on distance:
        `dist[r][c]` (implicitly tracked by BFS depth) depends on
         neighbours already visited at a smaller distance.
        """

        n = len(grid)
        dirs = [(1,0), (-1,0), (0,1), (0,-1)]   # 4-direction moves
        q = deque()

        # ---------- 1. Depth-first search: mark first island ----------
        def dfs(r, c):
            if r < 0 or r >= n or c < 0 or c >= n:     # outside grid
                return
            if grid[r][c] != 1:                        # already water or painted
                return
            grid[r][c] = 2                             # mark as part of first island
            q.append((r, c))                           # enqueue for later BFS
            for dr, dc in dirs:                        # visit 4 neighbours
                dfs(r + dr, c + dc)

        # find any land-cell to start DFS
        done = False
        for i in range(n):
            if done: break
            for j in range(n):
                if grid[i][j] == 1:
                    dfs(i, j)                # paint entire first island
                    done = True
                    break                       # exit loops once painted

        # ---------- 2. Breadth-first search: expand until 2nd island ----------
        steps = 0                               # BFS layer counter
        while q:
            for _ in range(len(q)):             # process one layer
                x, y = q.popleft()
                for dx, dy in dirs:
                    nx, ny = x + dx, y + dy
                    if 0 <= nx < n and 0 <= ny < n:
                        if grid[nx][ny] == 1:   # reached second island
                            return steps
                        if grid[nx][ny] == 0:   # water → flip & expand
                            grid[nx][ny] = 2
                            q.append((nx, ny))
            steps += 1                          # next ring costs +1 flip

        return -1   # (the grid always has two islands, so we never hit this)

