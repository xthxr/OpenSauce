/*
    Problem Statement :- Given a connected graph. Find the BFS of the graph starting from node 0.
    -----------------

    Algorithm :- 
    ---------
    1. Since it is a connected graph the algorithm is simple.
    2. Read the edges (u,v) in adjacency list.
    3. keep a visited array of size = no_of_nodes and initialize it to zero.
    4. Start from 0th node(Given in the question) otherwise prompt the node index and start from that node.
    5. maintain a queue and insert the starting node into the queue.
    6. initialize visited[node] = 1;
    7. Repeat the following while q not empty
        a. node_elem = the front element of queue.
        b. pop the front element from queue.
        c. add elem to the BFS list.
        d. for (auto it : adj[node_elem]) <== retreiving all the adjacent node of that node from adjacency list
           {
                visited[it] = 1;
                q.push(it)
           }
    8. Return the BFS list.


Example :- 
-------
Input :- 
-----
3 2
0 1
1 2

output :- 
------
0 1 2

How to run the program :- 
----------------------
create a file input.txt with input and then run the program. The output will be written in output.txt.


*/


#include<bits/stdc++.h>
using namespace std;


vector<int>BFSGraph(vector<int>adj[], int n)
{
    vector<int>vis(n,0);
    vector<int>bfs;

    vis[0] = 1;
    
    queue<int>q;
    q.push(0);

    while(!q.empty())
    {
        int node = q.front();
        q.pop();
        bfs.push_back(node);

        for(auto it : adj[node]){
            if(!vis[it])
            {
                vis[it] = 1;
                q.push(it);
            }
        }

    }

    return bfs;

}

int main()
{
    #ifndef ONLINE_JUDGE
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    #endif

    int n,m;
    cin >>n>>m;

    vector<int>adj[n];

    for(int i = 0; i < m; ++i) {
        int u, v;
        cin >> u >> v;
        adj[u].push_back(v);
        adj[v].push_back(u);
    }
    
    cout<<"BFS traversal starting from node 0"<<endl;
    for(auto it : BFSGraph(adj, n))
    {
        cout<<it<<" ";
    }

    return 0;
}