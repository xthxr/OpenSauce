//  https://leetcode.com/problems/maximum-subarray/
//problem 53




public class MaxSubArray { // in leetcode instead of this there is class solution 
    
    public static int maxSubArray(int[] arr) {
        int CS=0;int MS=Integer.MIN_VALUE;int count=0;int MS1=Integer.MIN_VALUE;
        for(int i=0;i<arr.length;i++){
            if(arr[i]>0){ count =1;break;}
            MS1=Math.max(MS1,arr[i]);
        }
        if(count==1){
            for(int i=0;i<arr.length;i++){
             
                CS+=arr[i];
                if(CS<0) CS=0;
                MS=Math.max(MS,CS);
            }
        }
        if(count ==1) return MS;
        else return MS1;
    }
        public static void main(String[] args) {
            int[] arr1 = {-2, 1, -3, 4, -1, 2, 1, -5, 4};
            int[] arr2 = {-5, -2, -8, -1}; // all negative test

            System.out.println("Maximum Subarray Sum (arr1): " + maxSubArray(arr1));
            System.out.println("Maximum Subarray Sum (arr2): " + maxSubArray(arr2));
        }
}

