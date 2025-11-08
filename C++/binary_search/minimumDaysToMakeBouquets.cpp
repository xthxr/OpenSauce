#include<bits/stdc++.h>
using namespace std;

// Problem: 
// Minimum number of Days to make m bouquets(Leetcode 1482)

// You are given an integer array bloomDay, an integer m and an integer k.

// You want to make m bouquets. To make a bouquet, you need to use k adjacent flowers from the garden.
// The garden consists of n flowers, the ith flower will bloom in the bloomDay[i] and then can be used in exactly one bouquet.

// Return the minimum number of days you need to wait to be able to make m bouquets from the garden. 
// If it is impossible to make m bouquets return -1.


// Example 1:
// Input: bloomDay = [1,10,3,10,2], m = 3, k = 1
// Output: 3

// Example 2:
// Input: bloomDay = [1,10,3,10,2], m = 3, k = 2
// Output: -1

// Example 3:
// Input: bloomDay = [7,7,7,7,12,7,7], m = 2, k = 3
// Output: 12


// Constraints:
// bloomDay.length == n
// 1 <= n <= 105
// 1 <= bloomDay[i] <= 109
// 1 <= m <= 106
// 1 <= k <= n


class Solution {
public:

    // let n= number of flowers 
    // let m= number of bouquets required
    // let k= number of flowers per bouquet 

    // m*k - total flowers required to make m bouquets
    
    // Approach:

    // - We need to find the minimum number of days to make at least m bouquets.
    // - If total flowers n < m*k, it's impossible.
    // - Use Binary Search on days from 1 to max(bloomDay).
    // - For each day, check if we can make m bouquets using that many days.
    // - Time complexity: O(n * log(max(bloomDay)))

    // maxi = maximum number of days required for any flower to bloom
    // days can go from low=1 high=maxi
    // binary search from low to high and find minimum number of days such that we can make bouquets
    // Time complexity for binary search - log₂(maxi) multiplied with time taken to check possibility for a day

    // for a particular number of days check the possibility if we can make m bouquets
    // flowers should be adjacent and should be bloomed to the given day 
    // if we have k adjacent flowers make a bouquet 
    // after counting possible number of bouquets check if there r minimum m 
    // Time complexity to check possibility of making minimum m bouquets - n 

    // Time Complexity: O(n log(max(bloomDay)))
    // Space Complexity: O(1)

    bool checkPossibility(vector<int> &bloomDay, int day, int k, int m){

        int n=bloomDay.size();
        int bouquetCount=0;
        int flowersCount=0;

        for(int i=0;i<n;i++){

            if(bloomDay[i]<=day){
                flowersCount++;

                if(flowersCount==k){
                    bouquetCount++;
                    flowersCount=0;
                }
            }
            else{
                flowersCount=0;
            }
        }

        if(bouquetCount>=m) return true;
        return false;
    }


    int minDays(vector<int> &bloomDay, int m, int k){

        int n=bloomDay.size();

        if(n<m*k) return -1;

        int maxi=bloomDay[0];

        for(int i=1;i<n;i++){
            maxi=max(maxi, bloomDay[i]);
        }

        if (!checkPossibility(bloomDay, maxi, k, m)) return -1;

        int low=1;
        int high=maxi;
        int min_days=maxi;

        while(low<=high){

            int mid=low+(high-low)/2;

            if(checkPossibility(bloomDay, mid, k, m)){
                min_days=mid;
                high=mid-1;
            }

            else{
                low=mid+1;
            }
        }

        return min_days;
    }
};

int main(){

    int n, m, k;
    cout<<"Enter number of flowers(n): ";
    cin>>n;
    
    vector<int> bloomDay(n);
    cout<<"Enter number of days of each flower: ";
    for(int i=0;i<n;i++){
        cin>>bloomDay[i];
    }

    cout<<"Enter number of bouquets (m) and flowers per bouquet (k): ";
    cin>>m>>k;

    Solution sol;
    int result=sol.minDays(bloomDay, m, k);

    if(result==-1){
        cout<<"It is impossible to make "<<m<<" bouquets."<<endl;
    }
    else{
        cout<<"Minimum number of days required: "<<result<<endl;
    }

    return 0;
}