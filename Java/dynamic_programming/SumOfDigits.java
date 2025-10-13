import java.util.Scanner;

public class SumOfDigits {
    static int sumOfDigits(int n) {
        if (n == 0)
            return 0;  // base case
        return (n % 10) + sumOfDigits(n / 10);  // recursive step
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        System.out.print("Enter a number: ");
        int num = sc.nextInt();

        int result = sumOfDigits(num);
        System.out.println("Sum of digits of " + num + " is: " + result);
        
        sc.close();
    }
}
