/*
    Problem Statement :-
    -----------------
    Given a weighted, connected graph, find the cost of the Minimum Spanning Tree (MST) using Prim’s Algorithm.

    Algorithm :-
    -----------
    1. Use a min-heap to pick the smallest edge that connects a visited node to an unvisited one.
    2. Keep a visited array to mark included vertices.
    3. For each chosen vertex, add all adjacent edges to the heap.
    4. Stop when all vertices are included.

    Example :-
    ----------
    Input :-
    5 6
    0 1 2
    0 2 4
    1 2 1
    1 3 7
    2 4 3
    3 4 2

    Output :-
    Total weight of MST: 8
*/

#include <bits/stdc++.h>
using namespace std;

int primMST(int V, vector<pair<int,int>> adj[]) {
    vector<int> vis(V, 0);
    priority_queue<pair<int,int>, vector<pair<int,int>>, greater<pair<int,int>>> pq;
    pq.push({0,0});
    int sum = 0;

    while(!pq.empty()) {
        auto [wt, node] = pq.top();
        pq.pop();
        if(vis[node]) continue;
        vis[node] = 1;
        sum += wt;

        for(auto it : adj[node]) {
            int adjNode = it.first;
            int edgeWt = it.second;
            if(!vis[adjNode]) pq.push({edgeWt, adjNode});
        }
    }
    return sum;
}

int main() {
    #ifndef ONLINE_JUDGE
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    #endif

    int V, E;
    cin >> V >> E;
    vector<pair<int,int>> adj[V];

    for(int i=0;i<E;i++) {
        int u,v,w;
        cin >> u >> v >> w;
        adj[u].push_back({v,w});
        adj[v].push_back({u,w});
    }

    cout << "Total weight of MST: " << primMST(V, adj);
    return 0;
}
