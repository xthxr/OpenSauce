//Implementing Graph Data Structure through Adjacency List
#include <iostream>
#include <vector>
#include <list>
#include <climits>
using namespace std;
class node
{
public:
    int data;
    int weight;
    node(int data, int weight) : data(data), weight(weight)
    {}
};
class Graph
{
public:
    vector<list<node>> G;
    int n;
    Graph(int n) : n(n), G(n)
    {}
    int weight(int u,int v)
    {
        for (auto node:G[u])
        {
            if(node.data==v)return node.weight; //If there exists edge between two vertices we return the weight of edge
        }
        return INT_MAX; //If there is no edge between two vertices we return INT_MAX
    }
    void addEdge(int u,int v,int w,bool undirected = true)
    {
        if(weight(u,v)!=INT_MAX)return; //Before adding a edge to a vertex we check if there already exists that edge
        G[u].push_back(node(v,w));
        if (undirected) //When graph is undirected 
            G[v].push_back(node(u,w));
    }
    void print()
    {

        for (int i = 0; i < n; i++)
        {
            cout << "Node " << i << " -> ";
            for (auto &node : G[i])
            {
                cout << "(" << node.data << ", w=" << node.weight << ") ";
            }
            cout << "\n";
        }
    }
};
int main()
{
    Graph G(5);
    //Testing by making some random graph
    for (int i = 0; i < 5; i++)
    {
        for (int j = 0; j < 5; j++)
        {
            if (i == j) 
            {
                continue;
            }
            if ((i + j) % 2 == 0)
            {
                G.addEdge(i,j,i+j+1);
            }
        }
    }
    G.print();
    return 0;
}

/*
Output
Node 0 -> (2, w=3) (4, w=5) 
Node 1 -> (3, w=5) 
Node 2 -> (0, w=3) (4, w=7) 
Node 3 -> (1, w=5) 
Node 4 -> (0, w=5) (2, w=7) 
*/

/*
Time Complexity analysis
T.C=O(1) to insert (without verifying edge between nodes exists or not)
T.C=O(V) to check whether a edge exists between nodes or not (Also to get their weights) [Atmost there can be v-1 edges for a node]

Space Complexity analysis
S.C=O(V+E) [As we are storing V vertices in form of array and there are E edges]
*/