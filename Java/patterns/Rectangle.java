package Java.patterns;

public class Rectangle {
    public static void main(String[] args) {
        int rows = 4; // Number of rows
        int columns = 6; // Number of columns

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < columns; j++) {
                System.out.print("* ");
            }
            System.out.println();
        }
    }
}
