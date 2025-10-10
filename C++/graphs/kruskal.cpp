/*
    Problem Statement :-
    -----------------
    Find the cost of the Minimum Spanning Tree (MST) using Kruskal’s Algorithm.

    Algorithm :-
    -----------
    1. Sort all edges by weight.
    2. Use Disjoint Set Union (DSU) to check for cycles.
    3. Pick the smallest edge that doesn’t form a cycle.
    4. Continue until V-1 edges are chosen.

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

struct DSU {
    vector<int> parent, rank;
    DSU(int n){ parent.resize(n); rank.resize(n,0); iota(parent.begin(), parent.end(), 0);}
    int find(int x){ return parent[x]==x?x:parent[x]=find(parent[x]); }
    void unite(int x,int y){
        x=find(x); y=find(y);
        if(x!=y){
            if(rank[x]<rank[y]) swap(x,y);
            parent[y]=x;
            if(rank[x]==rank[y]) rank[x]++;
        }
    }
};

int main() {
    #ifndef ONLINE_JUDGE
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    #endif

    int V,E;
    cin>>V>>E;
    vector<tuple<int,int,int>> edges;
    for(int i=0;i<E;i++){
        int u,v,w;
        cin>>u>>v>>w;
        edges.push_back({w,u,v});
    }

    sort(edges.begin(),edges.end());
    DSU dsu(V);
    int mstWt=0;

    for(auto [w,u,v]:edges){
        if(dsu.find(u)!=dsu.find(v)){
            dsu.unite(u,v);
            mstWt+=w;
        }
    }

    cout<<"Total weight of MST: "<<mstWt;
    return 0;
}
