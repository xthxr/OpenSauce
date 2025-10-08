package searching;

import java.util.Arrays;

public class SearchIn2dArray {
        public static void main(String[] args) {
            int[][] arr = new int[][] {
                    {23, 4, 1},
                    {18, 12, 3, 9},
                    {78, 99, 34, 56},
                    {18,12}
            };
            int target = 34;
            int[] ans = searching(arr,target); // format of return value {row, col}
            System.out.println(Arrays.toString(ans));
            System.out.println("Max element in array : " + maxElement(arr));
            System.out.println("Max element in array : " + max(arr));
        }
        static int[] searching(int[][] arr, int target) {
            for (int row = 0; row < arr.length ; row++) {
                for (int col = 0; col < arr[row].length; col++) {
                    if(arr[row][col]==target) {
                        return new int[]{row, col};
                    }
                }
            }
            return new int[]{-1, -1};
        }

        static int maxElement(int[][] arr) {
            int max = Integer.MIN_VALUE;
            for (int row = 0; row < arr.length ; row++) {
                for (int col = 0; col < arr[row].length; col++) {
                    if(arr[row][col] > max) {
                        max = arr[row][col];
                    }
                }
            }
            return max;
        }

        static int max(int[][] arr) {
            int max = Integer.MIN_VALUE;
            for (int[] ints : arr) {
                for (int element : ints) {
                    if (element > max) {
                        max = element;
                    }
                }
            }
            return max;
        }
}
