/**
 * Dungeon Game - Java implementation
 *
 * Problem (LeetCode 174 style): Given a 2D grid `dungeon` where each cell contains an integer
 * representing health change (negative = damage, positive = health), find the minimum initial
 * health required for the player (starting at top-left) to reach the bottom-right cell (princess)
 * such that health never drops to 0 or below at any point.
 *
 * Strategy: dynamic programming computed backwards from the destination. For each cell we compute
 * the minimum health required upon entering that cell so that we can reach the destination alive.
 *
 * Complexity: O(m * n) time and O(m * n) space (can be optimized to O(n)).
 */
public class DungeonGame {

    /**
     * Calculates the minimum initial health required to survive the dungeon.
     *
     * @param dungeon 2D integer array of health changes
     * @return minimum initial health (positive integer)
     * @throws IllegalArgumentException if dungeon is null or empty
     */
    public int calculateMinimumHP(int[][] dungeon) {
        if (dungeon == null || dungeon.length == 0 || dungeon[0].length == 0) {
            throw new IllegalArgumentException("dungeon must be a non-empty 2D array");
        }

        int m = dungeon.length;
        int n = dungeon[0].length;

        // dp[i][j] = minimum health required upon entering cell (i, j)
        // We allocate extra row/column filled with large values to simplify boundary checks
        final int INF = Integer.MAX_VALUE / 2; // avoid overflow
        int[][] dp = new int[m + 1][n + 1];

        for (int i = 0; i <= m; i++) {
            for (int j = 0; j <= n; j++) {
                dp[i][j] = INF;
            }
        }

        // Base cases: when we are just beyond the destination, we need at least 1 health to be "alive".
        dp[m][n - 1] = 1;
        dp[m - 1][n] = 1;

        for (int i = m - 1; i >= 0; i--) {
            for (int j = n - 1; j >= 0; j--) {
                int minNeededOnExit = Math.min(dp[i + 1][j], dp[i][j + 1]);
                int needed = minNeededOnExit - dungeon[i][j];
                dp[i][j] = Math.max(1, needed); // at least 1 hp to stay alive
            }
        }

        return dp[0][0];
    }

    // ===== Simple CLI / self-test harness =====
    public static void main(String[] args) {
        DungeonGame solver = new DungeonGame();

        // Example 1: classic example where answer is 7
        int[][] dungeon1 = {
            {-2, -3, 3},
            {-5, -10, 1},
            {10, 30, -5}
        };

        // Example 2: trivial positive-only dungeon -> initial health = 1
        int[][] dungeon2 = {
            {5, 1, 3},
            {2, 2, 1}
        };

        // Example 3: single cell negative
        int[][] dungeon3 = {{-10}};

        // Run tests using simple assertions. If assertions are disabled, prints results.
        runTest(solver, dungeon1, 7, "classic-example");
        runTest(solver, dungeon2, 1, "all-positive");
        runTest(solver, dungeon3, 11, "single-negative");

        System.out.println("All checks complete.");
    }

    private static void runTest(DungeonGame solver, int[][] dungeon, int expected, String name) {
        int result = solver.calculateMinimumHP(dungeon);
        if (expected == result) {
            System.out.println("[PASS] " + name + ": expected=" + expected + ", got=" + result);
        } else {
            System.err.println("[FAIL] " + name + ": expected=" + expected + ", got=" + result);
            // If you want tests to halt on failure, uncomment next line
            // System.exit(2);
        }
        // Additionally assert — helpful when running with -ea
        assert expected == result : name + " failed: expected=" + expected + ", got=" + result;
    }
}
