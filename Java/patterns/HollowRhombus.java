//Q Print a hollow rhombus
//         * * * * * 
//       *       *
//     *       *
//   *       *
// * * * * *

public class HollowRhombus {
  public static void main(String[] args) {
    int n=5;

for the spaces 
    for(int i=1; i<=n; i++) {
      for(int j=1; j<=(n-i); j++) {
        System.out.print(" ");
      }

      for the dots
      for(int j=1; j<=n; j++) {
        if(i==1||i==n||j==1||j==n) {
          System.out.print("* ");
        } else {
         System.out.print("  ");
        }
      }
      System.out.println();
    }
  }
}

public class Nestedloops {
  public static void main(String[] args) {
      for (int i = 0; i < 6; i++) {
          for (int j = 0; j < 7; j++) {
              if ((i == 0 && j % 3 != 0) || (i == 1 && j % 3 == 0) || (i - j == 2) || (i + j == 8)) {
                  System.out.print("*");
              } else {
                  System.out.print(" ");
              }
          }
          System.out.println();
      }
  }
}
