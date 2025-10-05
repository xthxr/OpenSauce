

public class LeftRotate {
    public static void main(String[] args) {
        int[] arr = {1,2,3,4,5};
        int k = 3;
        reverse(arr, 0, arr.length-1);
        reverse(arr, 0, k-1);
        reverse(arr, k, arr.length-1);

        for(int nums : arr){
            System.out.print(nums + " ");
        }
        
    }
    public static void reverse(int[] arr,int start,int end){
        while(start < end){
            int temp = arr[start];
            arr[start] = arr[end];
            arr[end] = temp;
            start++;
            end--;
        }
    }
}

// for right rotation first rotate last k elements n-k to n-1
// then rotate first n-k elements 0 to n-k-1
// then rotate whole array 0 to n-1
