// File: NumberGuessingGame.java

import java.util.Scanner;
import java.util.Random;

public class NumberGuessingGame {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        Random rand = new Random();

        System.out.println("Welcome to the Number Guessing Game!");
        System.out.println("Enter the maximum number for guessing range: ");
        int max = sc.nextInt();

        int numberToGuess = rand.nextInt(max) + 1;
        int attempts = 0;
        int guess;
        boolean guessed = false;

        System.out.println("I have picked a number between 1 and " + max + ". Try to guess it!");

        while (!guessed) {
            System.out.print("Enter your guess: ");
            guess = sc.nextInt();
            attempts++;

            if (guess < 1 || guess > max) {
                System.out.println(" Please enter a number within the range!");
                continue;
            }

            // Using if-else
            if (guess == numberToGuess) {
                guessed = true;
                System.out.println("🎉 Congratulations! You guessed the number in " + attempts + " attempts.");
            } else if (guess < numberToGuess) {
                System.out.println("Too low! Try a higher number.");
            } else {
                System.out.println("Too high! Try a lower number.");
            }

            // Bonus: give a hint using switch (optional complexity)
            if (!guessed) {
                int remainder = numberToGuess % 3;
                switch (remainder) {
                    case 0 -> System.out.println("Hint: The number is divisible by 3!");
                    case 1 -> System.out.println("Hint: When divided by 3, remainder is 1.");
                    case 2 -> System.out.println("Hint: When divided by 3, remainder is 2.");
                }
            }
        }

        sc.close();
    }
}
