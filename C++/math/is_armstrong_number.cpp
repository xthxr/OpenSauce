#include <string>
#include <iostream>
/**
 * @file is_armstrong_number.cpp
 * @brief Program to check whether a number is an Armstrong number.
 *
 * An Armstrong number (also known as a narcissistic number) is a number that is equal
 * to the sum of its own digits each raised to the power of the number of digits.
 * 
 * Example:
 *   153 = 1³ + 5³ + 3³ = 153
 *   370 = 3³ + 7³ + 0³ = 370
 *   1634 = 1⁴ + 6⁴ + 3⁴ + 4⁴ = 1634
 *
 * This program demonstrates how to compute and check Armstrong numbers in C++.
 */

/**
 * @brief Computes the power of a number (integer version).
 *
 * @param number The base number.
 * @param exponent The power to raise the number to.
 * @return The result of number raised to the exponent.
 */
int power(int number, int exponent) {
    int result = 1;
    for (int k = 0; k < exponent; k++) {
        result *= number;
    }
    return result;
}

/**
 * @brief Checks if a number is an Armstrong number.
 *
 * The function converts the number into a string to easily extract individual digits.
 * It then raises each digit to the power of the total number of digits and sums them.
 * If the resulting sum equals the original number, it is an Armstrong number.
 *
 * @param number The integer to test.
 * @return true if the number is an Armstrong number; false otherwise.
 */
bool numberIsArmstrong(int number) {
    int sum{0};

    // Convert number to string to count digits and access each digit
    std::string numberString = std::to_string(number);
    int length = numberString.length();

    for(int k = 0; k < length; k++) {
        int subTotal = power(numberString[k] - '0', length);
        sum = sum + subTotal;
    }

    return sum == number;
}

int main() {
    int numbers[] = {153, 370, 1634, 9474, 9475};

    for (int num : numbers) {
        if (numberIsArmstrong(num)) {
            std::cout << "The number " << num << " is Armstrong number." << std::endl;
        } else {
            std::cout << "The number " << num << " is NOT armstrong number." << std::endl;
        }
    }

    return 0;
}