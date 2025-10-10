/*
    Problem Statement :-
    -----------------
    Given a weighted directed graph, find the shortest distances between every pair of vertices using Floyd–Warshall Algorithm.

    Algorithm :-
    -----------
    1. Create a distance matrix initialized with given edge weights (and INF for no edge).
    2. Distance from node to itself = 0.
    3. For each vertex k:
         For each pair (i, j):
            dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j]);
    4. If dist[i][i] < 0 for any i, there’s a negative cycle.

    Example :-
    ----------
    Input :-
    4 5
    0 1 5
    0 3 10
    1 2 3
    2 3 1
    3 1 2

    Output :-
    Shortest distances matrix:
    0 5 8 9
    INF 0 3 4
    INF 3 0 1
    INF 2 5 0
*/

#include <bits/stdc++.h>
using namespace std;
#define INF 1000000000

int main(){
    #ifndef ONLINE_JUDGE
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    #endif

    int V,E;
    cin>>V>>E;
    vector<vector<int>> dist(V, vector<int>(V, INF));

    for(int i=0;i<V;i++) dist[i][i]=0;

    for(int i=0;i<E;i++){
        int u,v,w;
        cin>>u>>v>>w;
        dist[u][v]=w;
    }

    for(int k=0;k<V;k++)
        for(int i=0;i<V;i++)
            for(int j=0;j<V;j++)
                if(dist[i][k]<INF && dist[k][j]<INF)
                    dist[i][j]=min(dist[i][j], dist[i][k]+dist[k][j]);

    cout<<"Shortest distances matrix:\n";
    for(int i=0;i<V;i++){
        for(int j=0;j<V;j++){
            if(dist[i][j]==INF) cout<<"INF ";
            else cout<<dist[i][j]<<" ";
        }
        cout<<"\n";
    }

    return 0;
}
