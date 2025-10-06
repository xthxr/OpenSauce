package sorting;

// Kth smallest element using Quick Select
public class QuickSelectSort {
    static int ans;

    // Swap two elements in the array
    public static void swap(int[] arr, int i, int j){
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }

    // Print the array
    static void print(int[] arr){
        for(int ele : arr){
            System.out.print(ele + " ");
        }
        System.out.println();
    }

    // Partition around pivot: place pivot at correct index
    // Elements <= pivot to left, > pivot to right
    static int partition(int[] arr, int lo, int hi){
        int pivot = arr[lo];
        int pivotIdx = lo;
        int smallercount = 0;

        // Count elements <= pivot
        for(int i = lo + 1; i <= hi; i++){
            if(arr[i] <= pivot) smallercount++;
        }

        // Put pivot in its correct position
        int correctIdx = pivotIdx + smallercount;
        swap(arr, pivotIdx, correctIdx);

        // Rearranging elements around the pivot
        int i = lo, j = hi;
        while(i < correctIdx && j > correctIdx){
            if(arr[i] <= pivot) i++;
            else if(arr[j] > pivot) j--;
            else {
                swap(arr, i, j);
                i++;
                j--;
            }
        }

        return correctIdx;
    }

    // Quick Select to find k-th smallest element
    static void quickSelect(int[] arr, int lo, int hi, int k){
        if(lo > hi) return;
        if(lo == hi){
            if(lo == k - 1) ans = arr[lo];
            return;
        }

        int idx = partition(arr, lo, hi);
        if(idx == k - 1){
            ans = arr[idx];
            return;
        }

        if(k - 1 < idx) quickSelect(arr, lo, idx - 1, k);
        else quickSelect(arr, idx + 1, hi, k);
    }

    public static void main(String[] args) {
        int[] arr = {4, 9, 7, 1, 2, 3, 6, 5, 8};
        int n = arr.length;
        print(arr);
        int k = 3;
        ans = -1;
        quickSelect(arr, 0, n - 1, k);
        System.out.println(ans);  // Output: 3rd smallest element
    }
}