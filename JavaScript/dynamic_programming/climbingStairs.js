/**
 * Problem: Climbing Stairs
 *
 * You are climbing a staircase. It takes n steps to reach the top.
 * Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?
 *
 * This is a classic dynamic programming problem. The number of ways to reach step 'n'
 * is the sum of the ways to reach step 'n-1' (and taking one step) and the ways to
 * reach step 'n-2' (and taking two steps).
 * This forms the Fibonacci sequence.
 *
 * Recurrence Relation: ways(n) = ways(n-1) + ways(n-2)
 */

/**
 * Calculates the number of distinct ways to climb n stairs.
 * @param {number} n The total number of stairs. Must be a positive integer.
 * @returns {number} The number of distinct ways to climb.
 */
const climbStairs = (n) => {
  // Edge case: If there are 0 or 1 stairs, there's only one way to "climb".
  if (n <= 1) {
    return 1;
  }

  // We can solve this iteratively using a bottom-up approach to save space.
  // We only need to keep track of the last two values.
  
  // 'oneStepBack' stores the number of ways to reach the previous step (n-1).
  // Initialize for n=2 case, where ways(1) = 1.
  let oneStepBack = 1;

  // 'twoStepsBack' stores the number of ways to reach the step before that (n-2).
  // Initialize for n=2 case, where ways(0) = 1 (the base case).
  let twoStepsBack = 1;

  // We loop from step 2 up to the target step n.
  for (let i = 2; i <= n; i++) {
    // The number of ways to reach the current step 'i' is the sum of the previous two.
    const currentWays = oneStepBack + twoStepsBack;

    // Now, we update our pointers for the next iteration.
    // The previous `oneStepBack` becomes the new `twoStepsBack`.
    twoStepsBack = oneStepBack;
    // The `currentWays` we just calculated becomes the new `oneStepBack`.
    oneStepBack = currentWays;
  }

  // After the loop, `oneStepBack` holds the number of ways for step 'n'.
  return oneStepBack;
};

// --- Example Usage ---

console.log("--- Climbing Stairs Problem ---");

const stairs1 = 2;
const ways1 = climbStairs(stairs1);
console.log(`Number of stairs: ${stairs1}`);
console.log(`Distinct ways to climb: ${ways1}`); // Expected output: 2 (1+1, 2)
console.log("-" * 20);

const stairs2 = 5;
const ways2 = climbStairs(stairs2);
console.log(`Number of stairs: ${stairs2}`);
console.log(`Distinct ways to climb: ${ways2}`); // Expected output: 8
// Ways for 5 steps:
// 1. 1+1+1+1+1
// 2. 1+1+1+2
// 3. 1+1+2+1
// 4. 1+2+1+1
// 5. 2+1+1+1
// 6. 1+2+2
// 7. 2+1+2
// 8. 2+2+1
console.log("-" * 20);

const stairs3 = 10;
const ways3 = climbStairs(stairs3);
console.log(`Number of stairs: ${stairs3}`);
console.log(`Distinct ways to climb: ${ways3}`); // Expected output: 89
