class Solution {
    public int[] successfulPairs(int[] spells, int[] potions, long success) {
        // Step 1: Sort the potions array to use Binary Search later
        Arrays.sort(potions);

        // Array to store the count of successful pairs for each spell
        int ar[] = new int[spells.length];

        // Find the maximum potion strength (used for optimization check)
        int maxElement = 0;
        for (int i : potions) {
            if (i > maxElement) {
                maxElement = i;
            }
        }

        // Step 2: For each spell, check how many potions form a successful pair
        for (int i = 0; i < spells.length; i++) {

            // If even the strongest potion can't achieve success with this spell
            // then no need to check further (0 successful pairs)
            if ((long) spells[i] * maxElement * 1L >= success) {

                // Use Binary Search to find the first potion that forms a successful pair
                int index = bs(spells[i], potions, success);

                // All potions from this index to the end are successful
                ar[i] = potions.length - index;
            } else {
                // If condition not satisfied, then no potion can form a successful pair
                ar[i] = 0;
            }
        }

        // Return the result array containing successful pair counts for each spell
        return ar;
    }

    // Binary Search helper function
    // Finds the smallest index in potions[] where (spell * potion >= success)
    public int bs(int strength, int potions[], long success) {
        int lo = 0, hi = potions.length - 1;
        int ans = -1;  // To store the first valid potion index

        while (lo <= hi) {
            int mid = lo + (hi - lo) / 2;

            // If product >= success, store index and search in the left half
            if ((long) strength * potions[mid] * 1L >= success) {
                ans = mid;
                hi = mid - 1;  // Try to find an even smaller index
            } 
            // Else search in the right half
            else {
                lo = mid + 1;
            }
        }

        // Return the first valid index (or -1 if no valid potion found)
        return ans;
    }
}
