/* Sort Colors

Given an array nums with n objects colored red, white, or blue, sort them in-place so that objects of the same color are adjacent, with the colors in the order red, white, and blue.
We will use the integers 0, 1, and 2 to represent the color red, white, and blue, respectively.
You must solve this problem without using the library's sort function.

Example 1:
Input: nums = [2,0,2,1,1,0]
Output: [0,0,1,1,2,2]
Example 2:
Input: nums = [2,0,1]
Output: [0,1,2]

Constraints:
n == nums.length
1 <= n <= 300
nums[i] is either 0, 1, or 2.
 
Follow up: Could you come up with a one-pass algorithm using only constant extra space?
*/

/* Brute solution-

step 1- Run a loop to cnt number of 0,1,2:
for(int i=0;i<n;i++){
if(a[i]==0)cnt0++;
else if(a[i]==1)cnt1++;
else cnt2++;
}

step2- 
run 3 different loops to print 0,1,2;
for(int i=0;i<cnt0;i++)a[i]=0;
for(int i=cnt0;i<cnt0+cnt1;i++)a[i]=1;
for(int i=cnt0+cnt1;i<n;i++)a[i]=2;

TC- O(2*n); SC-O(1);
*/



/* Optimal Solution- DUTCH NATIONAL FLAG ALGORITHM:
Explanation:
-Take 3 pointers- low,mid,high
lets divide the array into parts:
part1= 0 to low-1;
part2= low to mid-1;
part3=mid to high;
part4=high+1 to n-1;

-Remember 3 rules: Everything from 0 to low-1= 0 
                   Everything from low to mid-1= 1 
                   Everything from high+1 to n-1= 2 

according to this the only unsorted part will be from mid to high:
so when you begin and your entire array is unsorted, the first element is mid and last element is high;

Now, lets say the mid was a 0, everything before it should be 0 according to the rules, so isn't mid a part of that part1 so swap low and mid and low++, mid++;
If mid is 1, it is already sorted -> mid++;
eg- 0,0,1,1,0,1,2,2;
   swap- 0,0,0,1,1,1,2,2;    

Similarly if mid=2; it needs to go to part3 so swap it with high, high--;

*/

#include <bits/stdc++.h>
using namespace std;
int main(){
  int n; cin>>n;
  vector<int>nums(n);
  for(int i=0;i<n;i++)cin>>nums[i];

  int low=0,mid=0,high=n-1;

  while(mid<=high){
    //mid=0; should be in part 1 so swap with low and then inc low and mid;
    if(nums[mid]==0){
      swap(nums[low],nums[mid]);
      low++; mid++;
    }

    //mid=1 means its already sorted, just inc mid;
    else if(nums[mid]==1)mid++;

    //mid=2 then swap with high and dec high while keeping mid at same place
    else{
      swap(nums[high],nums[mid]); high--;
    }
  }

  for(int i=0;i<n;i++)cout<<nums[i]<<" ";
  cout<<endl;

}

//TC= O(N)
//SC= O(1)
