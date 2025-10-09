/*
Amazon SDE 2025 – Online Assessment Question

Problem:
Amazon Kindle has several e-books that customers can purchase directly.
There are n books ordered sequentially numbered 1 to n, where the i-th book has a cost of cost[i].

A customer wants to purchase all the books, and Kindle offers a unique discount to minimize the total cost.

Discount options:
- Buy the leftmost book for cost[i]
- Buy the rightmost book for cost[j]
- Buy both leftmost and rightmost books together for pairCost (can be used up to k times)

Goal:
Return the minimum total cost to buy all books.

Function Signature:
long findMinPrice(List<Integer> cost, int pairCost, int k)

Sample Input:
5
9 11 13 15 17
6
2

Sample Output:
21

Explanation:
Buy book 1 individually → 9
Buy books 2 & 5 together → 6
Buy books 3 & 4 together → 6
Total = 21
*/

import java.util.*;

public class AmazonKindleDiscount {
    public static long findMinPrice(List<Integer> cost, int pairCost, int k) {
        int left = 0, right = cost.size() - 1;
        long total = 0;

        while (left <= right) {
            if (left == right) {
                total += cost.get(left);
                break;
            }

            int sum = cost.get(left) + cost.get(right);
            if (k > 0 && pairCost < sum) {
                total += pairCost;
                k--;
                left++;
                right--;
            } else {
                if (cost.get(left) <= cost.get(right)) {
                    total += cost.get(left);
                    left++;
                } else {
                    total += cost.get(right);
                    right--;
                }
            }
        }

        return total;
    }
}
