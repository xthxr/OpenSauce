public class InterpolationSearch {

    public static void main(String[] args) {

        // Interpolation search is an improvement over binary search.
        // It is most effective for uniformly distributed data.
        // The algorithm "guesses" the position of a value based on calculation.
        // If the guess is incorrect, the search area is narrowed, and a new guess is calculated.

        //						  average case: O(log(log(n)))
        //						  worst case: O(n) [values increase exponentially]

        System.out.println("-------- Search with distributed data --------");
        int[] array = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        int target = 8;
        int index = getInterpolationSearch(array, target);
        if(index != -1){
            System.out.println("Element found at index: " + index);
        } else {
            System.out.println("Element not found");
        }

        System.out.println("-------- Data with exponential growth --------");
        array = new int[]{1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024};
        target = 256;
        index = getInterpolationSearch(array, target);
        if(index != -1){
            System.out.println("Element found at index: " + index);
        } else {
            System.out.println("Element not found");
        }

    }

    private static int getInterpolationSearch(int[] array, int value) {
        int high = array.length - 1;
        int low = 0;

//       The loop runs as long as the target is still possible within the range [low...high]
        while (value >= array[low] && value <= array[high] && low <= high) {

//            This is the main formula. “Guess” the target's position based on its relative distance
//            within the current array value range, not just splitting it in half.
            int probe = low + (high - low) * (value - array[low])/(array[high] - array[low]);

            System.out.println("probe: " + probe);

            if(array[probe] == value){
                return probe;

//                If the guess is too small, discard the left area.
            } else if(array[probe] < value) {
//                Continue searching in the right area.
                low = probe + 1;

//                If the guess is too big, discard the right area.
            } else {
//                Continue searching in the left area.
                high = probe - 1;
            }
        }

        return -1;
    }

}
