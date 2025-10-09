# 547. Number of Provinces

## Problem Description

There are `n` cities. Some of them are connected, while some are not. If city `a` is connected directly with city `b`, and city `b` is connected directly with city `c`, then city `a` is connected indirectly with city `c`.

A **province** is a group of directly or indirectly connected cities and no other cities outside of the group.

You are given an `n x n` matrix `isConnected` where `isConnected[i][j] = 1` if the `ith` city and the `jth` city are directly connected, and `isConnected[i][j] = 0` otherwise.

Return the total number of provinces.

## Examples

### Example 1:
```
Input: isConnected = [[1,1,0],[1,1,0],[0,0,1]]
Output: 2
```
**Explanation:** Cities 0 and 1 are connected forming one province. City 2 is isolated, forming another province.

### Example 2:
```
Input: isConnected = [[1,0,0],[0,1,0],[0,0,1]]
Output: 3
```
**Explanation:** All three cities are isolated from each other, so there are 3 provinces.

## Constraints

- `1 <= n <= 200`
- `n == isConnected.length`
- `n == isConnected[i].length`
- `isConnected[i][j]` is `1` or `0`
- `isConnected[i][i] == 1` (each city is connected to itself)
- `isConnected[i][j] == isConnected[j][i]` (symmetric matrix)

---

## Solution Approach & Intuition

This is a classic **Connected Components** problem in graph theory. The key insight is to treat cities as nodes and direct connections as edges in an undirected graph.

### Approach: Depth-First Search (DFS)
**Time Complexity:** O(N²) | **Space Complexity:** O(N)

### Intuition

1. **Graph Representation**: The `isConnected` matrix represents an adjacency matrix of an undirected graph
2. **Connected Components**: Each province is essentially a connected component in this graph
3. **DFS Traversal**: Use DFS to explore all cities reachable from a starting city
4. **Counting**: Each time we start a new DFS (from an unvisited city), we've found a new province

### Algorithm Steps

1. **Initialize**: Create a visited array to track explored cities
2. **Iterate**: Go through each city (0 to n-1)
3. **New Province**: If a city hasn't been visited, it's the start of a new province
   - Increment province count
   - Perform DFS to mark all connected cities as visited
4. **DFS Logic**: For the current city, explore all directly connected unvisited cities recursively

### Key Components

#### DFS Function
```cpp
void dfs(int node, vector<vector<int>>& grid, vector<int>& vis){
    vis[node] = 1;  // Mark current city as visited
    
    // Check all other cities
    for(int i = 0; i < grid.size(); i++){
        // If city i is connected to current city and not visited
        if(grid[node][i] == 1 && !vis[i]){
            dfs(i, grid, vis);  // Recursively visit city i
        }
    }
}
```

#### Main Algorithm
```cpp
int findCircleNum(vector<vector<int>>& isConnected) {
    int n = isConnected.size();
    int count = 0;  // Province counter
    vector<int> vis(n, 0);  // Visited array
    
    for(int i = 0; i < n; i++){
        if(!vis[i]){  // If city i not visited
            count++;  // New province found
            dfs(i, isConnected, vis);  // Mark all connected cities
        }
    }
    return count;
}
```

### How It Works

1. **Start with city 0**: If unvisited, it's a new province
2. **DFS from city 0**: Mark all cities reachable from city 0 as visited
3. **Move to next unvisited city**: This represents a new disconnected component (province)
4. **Repeat**: Continue until all cities are visited

### Example Walkthrough

For `isConnected = [[1,1,0],[1,1,0],[0,0,1]]`:

1. **Start with city 0** (unvisited) → Province 1
   - DFS(0): Mark city 0 as visited
   - City 0 connects to city 1 → DFS(1)
   - City 1 connects to city 0 (already visited)
   - No more connections from cities 0 or 1

2. **Move to city 1** (already visited) → Skip

3. **Move to city 2** (unvisited) → Province 2
   - DFS(2): Mark city 2 as visited
   - City 2 only connects to itself

**Result**: 2 provinces

### Alternative Approaches

This problem can also be solved using:
- **BFS (Breadth-First Search)**: Similar logic with queue-based traversal
- **Union-Find (Disjoint Set Union)**: Efficient for dynamic connectivity queries
- **Matrix Manipulation**: Using matrix powers (less efficient)

## Implementation

The complete DFS solution can be found in the `547-Number-Of-Provinces.cpp` file in this directory.