//   https://leetcode.com/problems/spiral-matrix/
//Problem 54

import java.util.ArrayList;
import java.util.List;

public class SpiralMatrix {  // in leetcode instead of this there is class solution 
    
    public static List<Integer> spiralOrder(int[][] matrix) {
        List<Integer> result= new ArrayList<>();
        int sr=0;
        int er=matrix.length-1;
        int sc=0;
        int ec=matrix[0].length-1;
        while(sr<=er && sc<=ec){
                //top
                for(int i=sc;i<=ec;i++){
                    result.add(matrix[sr][i] );
                }
            //right
            for(int j=sr+1;j<=er;j++){
                result.add(matrix[j][ec]);
            } 
            //bottom
            for( int k = ec-1;k>=sc ;k--){
                if(sr==er) break;
                result.add(matrix[er][k]);
            }
            //left
            for(int l=er-1;l>=sr+1;l--){
                if(sc==ec) break;
                result.add(matrix[l][sc]);
            }
            sr++;
            er--;
            sc++;
            ec--;
        }
        return result;
    }
    public static void main(String[] args) {
   int matrix[][] = {
            {1, 2, 3, 4},
            {5, 6, 7, 8},
            {9, 10, 11, 12},
            {13, 14, 15, 16}
        };
          // Call the method and store result
        List<Integer> spiralList = spiralOrder(matrix);

        // Print the list
        System.out.println(spiralList);
    }
}
    

    

