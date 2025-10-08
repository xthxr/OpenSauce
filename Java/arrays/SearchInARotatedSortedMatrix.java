// Leetcode Problem 33
// https://leetcode.com/problems/search-in-rotated-sorted-array/description/


public class SearchInARotatedSortedMatrix { // in leetcode instead of this it is written class Solution

    public static int search(int arr[],int tar, int si,int ei){
        if(si>ei) return -1; // -1 is an invalid index
        int mid=si+(ei-si)/2;
        if(arr[mid]== tar) return mid;
        // mid on line 1
        if(arr[si]<=arr[mid]){
            //case a: left part of line 1  
            if(arr[si]<=tar && tar<=arr[mid]) return search(arr, tar, si, mid -1);
            // case b: right side of line 1 and  it can be either in RS of line 1 or in line 2 hence here it  should be "side" not "part" here case a  should be part
            else  return search(arr, tar, mid +1, ei);
        }
        // mid on line 2
        else{ //arr[mid]<=arr[ei] // is correct not arr[si]<arr[mid]
            // case c : right part of line 2
            if(arr[mid]<=tar && tar<=arr[ei]) return search(arr, tar, mid +1,ei);
            //case d : left side of line 2 that is it can be either in LS of line 2 or in line 1
            else return search(arr, tar, si, mid-1);
        }

    }
    public static void main(String[] args) {
        int arr[]={4,5,6,7,0,1,2};
        int tar=0;
        int tIdx=search(arr, tar, 0, arr.length-1);
        System.out.println(tIdx);
    }
}

