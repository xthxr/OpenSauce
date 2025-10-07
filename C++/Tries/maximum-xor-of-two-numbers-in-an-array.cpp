/* Problem: Leetcode 421. Maximum XOR of Two Numbers in an Array

Given an integer array nums, return the maximum result of nums[i] XOR nums[j], where 0 <= i <= j < n.

Example 1:
Input: nums = [3,10,5,25,2,8]
Output: 28
Explanation: The maximum result is 5 XOR 25 = 28.

Example 2:
Input: nums = [14,70,53,83,49,91,36,80,92,51,66,70]
Output: 127

*/
#include <bits/stdc++.h>
using namespace std;

struct Node{
    // contain 2 bits 0 and 1
    Node *list[2];
    //bool flag;

    bool contain(int i){
        return (list[i]!=NULL);
    }

    void put(int i){
        list[i]=new Node();
    }

    Node * get(int i){
        return list[i];
    }
};

class Trie{
public:
    Node *root;
    
    Trie(){
        root=new Node();
    }

    void insert(int num){
        Node *node=root;
        for(int i=31;i>=0;i--){
            int bit=(num&(1<<i)?1:0);
            if(!node->contain(bit)){
                node->put(bit);
            }
            node=node->get(bit);
        }
    }

    int maxxor(int noo){
        Node *node=root;

        int ans=0;
        for(int i=31;i>=0;i--){
            int tofindbit=!(noo&(1<<i)?1:0);
            // so we need to check if this bit exists or not
            // if exists then xor ans bit will be 1
            if(!node->contain(tofindbit)){
                // if not found then we go to the other bit
                node=node->get(!tofindbit);
            }
            else{
                node=node->get(tofindbit);
                ans=(ans|(1<<i));
            }
        }
        return ans;
    }
};
    
class Solution {
public:
    int findMaximumXOR(vector<int>& nums) {
        // using the trie data structure

        Trie trie;
        for(int i:nums){
            trie.insert(i);
        }

        int ans=0;
        for(int i:nums){
            ans=max(ans,trie.maxxor(i));
        }
        return ans;
    }
};