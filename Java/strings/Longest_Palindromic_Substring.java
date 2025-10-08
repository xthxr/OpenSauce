import java.io.*;
import java.util.*; 
// Longest Palindromic Substring is a classic problem that can be solved using the expand around center technique.
// The idea is to consider each character (and the gap between characters) as a potential center of a palindrome
// and expand outwards as long as the characters on both sides are equal.
public class Longest_Palindromic_Substring {
    public static int lps(String str,int n){
        int max=0; // to store the maximum length of palindrome found
        int tmax=0; // to store the overall maximum length of palindrome
        for(int i=0;i<n;i++){
            int len1=expand(i,i+1,str,n);     // for even length palindrome
            int len2=expand(i,i,str,n);   // for odd length palindrome
            max=Math.max(len1,len2);
            tmax=Math.max(tmax,max);
        }
        return tmax;    // return the length of the longest palindromic substring
    }
    public static int expand(int p1,int p2,String str,int n){
        while((p1>=0 && p2<n) && (str.charAt(p1)==str.charAt(p2))){
                p1--; // expand to the left
                p2++;    // expand to the right
        }
        return p2-p1-1; // return the length of the palindrome
    }
    public static void main(String[] args) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Main. */
        Scanner sc=new Scanner(System.in);
        System.out.println("Enter the number of test cases:");
        int t=sc.nextInt();
        sc.nextLine();
        while(t-- > 0){ // for multiple test cases
            System.out.println("Enter the length of the string:");
            int n=sc.nextInt(); // length of the string
            System.out.println("Enter the string:");
            String s=sc.next(); // input string
            System.out.println("Length of the longest palindromic substring is:");
            System.out.println(lps(s,n)); // print the length of the longest palindromic substring
        }
    }
}