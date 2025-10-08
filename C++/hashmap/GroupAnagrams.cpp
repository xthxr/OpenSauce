/*
Problem Statement :- Given an array of different strings. Group the anagrams and then return the
-----------------   list of grouped strings.

Anagrams :- Two words NAT and TAN are anagrams of each others if they have same set of characters
--------    in them.

Algorithm :- 
---------
1. Create an hash map of type key(string) and values list(string)
2. Iterate through each word of the array.
3. Sort each word (since the word is a string it is a list of characters as well)
4. add the word into the map[sorted_word]
5. Finally iterate through all the elements of the map and return the list of values.


Input :- 
-----
6
eat tea tan ate nat bat

Output :-
------
<=========================Group Anagrams=====================>
bat 
tan nat 
eat tea ate 


How to run this code :- 
--------------------
1. create an input.txt file and add inputs
2. compile and run the program file
3. output will be observed in output.txt file.

*/

#include<bits/stdc++.h>
using namespace std;

vector<vector<string>>GroupAnagrams(vector<string>s)
{
    unordered_map<string,vector<string>>mpp;

    for(auto elem : s)
    {
        string word = elem;
        sort(word.begin(),word.end());
        mpp[word].push_back(elem);
    }

    vector<vector<string>>ans;
    for(auto it : mpp)
    {
        ans.push_back(it.second);
    }

    return ans;
}



int main()
{
    #ifndef ONLINE_JUDGE
    freopen("input.txt","r",stdin);
    freopen("output.txt","w",stdout);
    #endif

    vector<string>s;
    int n;
    cin >> n;
    for(int i = 0 ; i < n ; i++)
    {
        string inp;
        cin >> inp;
        s.push_back(inp);
    }

    cout<<"<=========================Group Anagrams=====================>"<<endl;
    for(auto it : GroupAnagrams(s))
    {
        for(auto e : it)
        {
            cout<<e<<" ";
        }
        cout<<endl;
    }


    return 0;
}
