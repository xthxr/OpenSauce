/*
Problem statement:- Given a graph perform DFS on it.
-----------------

Algorithm :- 
---------
Algorithm Steps:

1. Build the adjacency list:
  a. For each edge (u, v):
  b. Add v to the adjacency list of u.
  c. Add u to the adjacency list of v.
2. Create a visited array of size n initialized with 0.
3. Create an empty vector dfs to store the traversal order.
4. DFS Function (DFS(node, adj, vis, dfs)):
5. Mark node as visited.
6. Add node to the DFS traversal list.
7. For every connected node it of node:
  a. If it is not visited, recursively call DFS(it, adj, vis, dfs).
  b. Start traversal:
     (i) Call DFS(0, adj, vis, dfs) to begin DFS from node 0.

Input :- 
-----
4 4
0 1
1 2
2 3
3 0

Output :- 0 1 2 3
------
How to run the program :- 
----------------------
create a file input.txt with input and then run the program. The output will be written in output.txt.


*/


#include<bits/stdc++.h>
using namespace std;

void DFS(int node , vector<int>adj[],vector<int>vis, vector<int>&dfs)
{
    vis[node] = 1;
    dfs.push_back(node);
    for(auto it : adj[node])
    {
        if(!vis[it]){
            DFS(it,adj,vis,dfs);
        }
    }
}

vector<int>DFSGraph(int n,vector<int>adj[])
{
    vector<int>vis(n,0);
    vector<int>dfs;

    int node = 0;

    DFS(node,adj,vis,dfs);
    return dfs;
}



int main()
{
    #ifndef ONLINE_JUDGE
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    #endif

    int n,m;
    cin >> n >> m;

    vector<int>adj[n];

    for(int i = 0 ; i < m ; i++)
    {
        int u,v;
        cin >> u >> v;
        adj[u].push_back(v);
        adj[v].push_back(u);
    }
    cout<<"Depth first traversal of the graph starting from node 0. "<<endl;

    for(auto it :DFSGraph(n,adj))
    {
        cout<<it<<" ";
    }

    return 0;
}