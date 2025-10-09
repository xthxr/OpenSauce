#include <bits/stdc++.h>
using namespace std;
 class Solutions{
    public:
    void dfs(int node, vector<vector<int>>& grid, vector<int>& vis){
      vis[node]=1;
      for(int i=0; i<grid.size(); i++){
        if(grid[node][i]==1 && !vis[i]){
          dfs(i, grid, vis);
        }
      }
    }
    int findCircleNum(vector<vector<int>>& isConnected) {
        int n= isConnected.size();
        int count=0;
        vector<int> vis(n,0);
        for(int i=0; i<n; i++){
          if(!vis[i]){
            count++;
            dfs(i, isConnected, vis);
          }
        }
        return count;
    }
     
 }