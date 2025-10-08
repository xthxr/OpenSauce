#include <bits/stdc++.h>
using namespace std;
class Solution{
    public:
// // Most Naive method-> TC=O(N^2) SC=O(1)
    int majorityElement(vector<int>& nums) {
      int n= nums.size();
        for(int i=0; i<n; i++){
          int count=0;
           for(int j=0; j<n; j++){
             if(nums[i]==nums[j]){
               count++;
             }
            if(count>n/2)return nums[i];
          }
         }
        return 0;
     }

// Better Approach(Hashing) TC=O(NlogN) SC=O(N)
   int majorityElement(vector<int>& nums) {
   int n= nums.size();
  map<int, int> mpp;
   for(int i=0; i<n; i++){
     mpp[nums[i]]++;
   }
  for(auto it:mpp){
    if(it.second>n/2){
       return it.first;
     }
    
   }
  return -1;

// Most Optimized(Moore's Voting Algorithm)
    int majorityElement(vector<int>& nums) {
      int count = 0;
    int candidate = 0;

    for (int num : nums) {
        if (count == 0) {
            candidate = num;
        }
        count += (num == candidate) ? 1 : -1;
    }

    return candidate;
  }
}
};

