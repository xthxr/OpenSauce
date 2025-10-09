import java.util.*;
// Java program to find the floor value of a given element in a sorted array using Binary Search
// Time Complexity: O(log n) where n is the number of elements in the array
// Space Complexity: O(1)

public class FloorBinarySearch {
    static int floor(int arr[],int n,int x){
        int low=0,high=n-1;
        int ans=Integer.MIN_VALUE; // Initialize answer to minimum value
        while(low<=high){
            int mid=(low+high)/2;   //middle index
            if(arr[mid]==x) return arr[mid];
            else if(arr[mid]<x){  
                ans=arr[mid];
                low=mid+1;
            }
            else high=mid-1;
        }
        return ans;
    }
    public static void main(String[] args) {
        Scanner sc=new Scanner(System.in);
        System.out.println("Enter the number of test cases:");
        int t=sc.nextInt();
        while(t-- > 0){
        System.out.println("Enter the size of array:");
        int n=sc.nextInt();
        System.out.println("Enter the number of elements in the array:");
        int[] arr=new int[n];
        for(int i=0;i<n;i++){
            arr[i]=sc.nextInt();
        }
        Arrays.sort(arr);
        System.out.println("Enter the element whose floor value is to be found:");
        int x=sc.nextInt();
        
        System.out.println("The Value is : " +floor(arr,n,x));
        }
    }
        
    }

