// Program to check if one string is rotation of another
// Folder: strings/

import java.util.Scanner;

public class CheckStringRotation {
    public static boolean isRotation(String s1, String s2) {
        if (s1.length() != s2.length() || s1.length() == 0)
            return false;

        String combined = s1 + s1;
        return combined.contains(s2);
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);

        System.out.print("Enter first string: ");
        String s1 = sc.nextLine();

        System.out.print("Enter second string: ");
        String s2 = sc.nextLine();

        if (isRotation(s1, s2))
            System.out.println(" \"" + s2 + "\" is a rotation of \"" + s1 + "\"");
        else
            System.out.println(" \"" + s2 + "\" is NOT a rotation of \"" + s1 + "\"");

        sc.close();
    }
}
