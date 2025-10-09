/*
    Problem Statement :-
    -----------------
    Given a weighted, connected graph, find the shortest distance from the source node (0) to all other nodes using Dijkstra's Algorithm.

    Algorithm :-
    -----------
    1. Create an adjacency list where each entry adj[u] contains pairs (v, weight).
    2. Initialize a distance array 'dist' with infinity (INT_MAX).
    3. Use a min-priority queue (min-heap) to always pick the node with the smallest known distance.
    4. Set dist[source] = 0 and push (0, source) into the priority queue.
    5. While the queue is not empty:
        a. Extract the node with the smallest distance.
        b. For each neighbor (v, w), if dist[u] + w < dist[v], update dist[v] and push (dist[v], v) into the queue.
    6. Return the distance array.

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
    Shortest distances from node 0:
    0 2 3 9 6
*/

#include <bits/stdc++.h>
using namespace std;

vector<int> dijkstra(int V, vector<pair<int,int>> adj[], int src) {
    vector<int> dist(V, INT_MAX);
    priority_queue<pair<int,int>, vector<pair<int,int>>, greater<pair<int,int>>> pq;

    dist[src] = 0;
    pq.push({0, src});

    while(!pq.empty()) {
        int node = pq.top().second;
        int d = pq.top().first;
        pq.pop();

        for(auto it : adj[node]) {
            int adjNode = it.first;
            int wt = it.second;

            if(d + wt < dist[adjNode]) {
                dist[adjNode] = d + wt;
                pq.push({dist[adjNode], adjNode});
            }
        }
    }
    return dist;
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

    vector<int> dist = dijkstra(V, adj, 0);

    cout << "Shortest distances from node 0:\n";
    for(int i=0;i<V;i++) cout << dist[i] << " ";
    return 0;
}
