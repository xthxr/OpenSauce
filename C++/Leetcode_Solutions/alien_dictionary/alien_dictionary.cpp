/*
Problem statement :- Given a sorted dictionary of an Alien language having N words and K starting alphabets of standard dictionary. Find the order of the
-----------------    characters in the dictionary. Many order might be possible for any test case.

Inputs :-   N=5, K=4
------      ["baa","abcd","abca","cab","cad"]

Output :- [ "b","d","a","c" ]
------

Explanation :- 
-----------
The given words ae in the sorted manner. Therefore from there itself we can say that there is some order of appearing of an alphabet in the dictionary.

In standard english dictionary, "abca" appears before "abcd"
-                                ---------------------------
- because first three letters "a","b" and "c" are same in both but in last letters "a" and "d", "a" appears before "d" in standard english dictionary.

In the same manner in Alien dictionary if in sorted list "baa" appears before "abcd" then in that case on comparing "b" of "baa" with "a" of "abcd"
since both are not same we can say that "b" appears before "a" in the dictionary.
in the same manner if we compare "abcd" with "abca" since "abcd" appears before "abca" in the given sorted list, on comparing each and every letter,
we can say that first three letters are same but the last letters are different therefore "d" appears before "a".
So all we have to do is compare a[index]th string and a[index + 1]th string and compare the values to find the differentiating letter which will help
us to determine possibility of the occurence of the letter.
But from there also we cannot determine the order. So we need to create a Directed graph out of it (the graph will be DAG always).

Then we will perform the topological sort since it gives linear ordering of the vertices (v,u) where v appears before u in the linear ordering.
-----------------------------------------

-----------------------------------------------------------------------------------------------------------------------------------------------------------|
What if the dictionary is wrong then how to check if it is wrong?                                                                                          |
================================================================                                                                                           |
If the dictionary is wrong then there will always be a cycle in the directed graph formed for topological sort.                                            |
So check the list of the values that are added from queue during topo sort and if size_of_list != no_of_alphabets then there is a cycle and dict is wrong. |
-----------------------------------------------------------------------------------------------------------------------------------------------------------|
*/

#include<bits/stdc++.h>
using namespace std;

vector<int>TopoSortOnAlienDict(vector<vector<int>>adj,int K,vector<int>Indegree)
{   
    vector<int>order;
    queue<int>q;
    for(int i = 0 ; i < K; i++)
    {
        if(Indegree[i] == 0)
        {
            q.push(i);
        }
    }

    while(!q.empty())
    {
        int node = q.front();
        q.pop();
        order.push_back(node);

        for(auto adj_nodes : adj[node])
        {
            Indegree[adj_nodes]--;
            if(Indegree[adj_nodes] == 0)
            {
                q.push(adj_nodes);
            }
        }
        
    }

    return order;

}




vector<int> createAdj(vector<string>dict,int N,int K)
{
    vector<vector<int>>adj(K);
    for(int i = 0 ; i < N-1; i++)
    {
        string s1 = dict[i];
        string s2 = dict[i+1];

        int length = min(s1.size(),s2.size());
        for(int i = 0 ; i<length ; i++)
        {
            if(s1[i]!=s2[i])
            {
                adj[s1[i]-'a'].push_back(s2[i]-'a');
                break;
            }
        }
    }

    vector<int>Indegree(K,0);

    for(int i = 0 ; i < K ; i++)
    {
        for(auto adj_node : adj[i])
        {
            Indegree[adj_node]++;
        }
    }

    return TopoSortOnAlienDict(adj,K,Indegree);
    
}



int main()
{
    #ifndef ONLINE_JUDGE
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    #endif

    int N,K;
    cin >> N >> K;

    vector<string>dict;
    for(int  i = 0 ; i < N ; i++)
    {
        string s;
        cin>>s;
        dict.push_back(s);
    }

    cout<<"<===== Alien Dictionary using Topological Sort =====>"<<endl;
    for(auto it : createAdj(dict,N,K))
    {
        cout << char(it + 'a') << " ";
    }

    return 0;
}