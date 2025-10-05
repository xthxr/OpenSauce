/**
 * simple yet iterative merge sort :)
 * this sorts an array and returns to a new sorted array using bottom-up merge sort.
 * 
 * Complexity
 * -----------
 * Time: O(n log n)   : log2n merge passes over n elements
 * Space: O(n)        : uses one auxiliary buffer of length n
 * 
 * 
 * Example usage and its result
 * -----------
 * console.log(mergeSort([38, 27, 43, 10]));                    => [10, 27, 38, 43]
 * console.log(mergeSort([38, 27, 43, 10], true));              => [43, 38, 27, 10]
 * console.log(mergeSort(['dog', 'buffalo', 'cat', 'alien']));  => ["alien", "buffalo", "cat", "dog"]
 */

const mergeSort = (a, c = false) => {
  const len = a.length;
  if (len < 2) return [...a];

  // variables to make the array untouched and swap in right orders
  let src = [...a];
  let base = new Array(len);

  // w as width, l as left, m as middle, r as right
  for (let w = 1; w < len; w <<= 1) {
    for (let l = 0; l < len; l += (w << 1)) {
      const m = Math.min(l + w, len);
      const r = Math.min(l + (w << 1), len);

      // check condition what user wants for its order
      const order = c
        ? (a, b) => a > b  // desc
        : (a, b) => a < b; // asc

      // merge left-middle and middle-right to base
      let i = l, j = m, k = l;
      while (i < m && j < r) {
        // if equal, take from <src[i]> so the original order stays
        if (order(src[i], src[j]) || src[i] === src[j]) base[k++] = src[i++];
        else base[k++] = src[j++];
      }

      while (i < m) base[k++] = src[i++];
      while (j < r) base[k++] = src[j++];
    }

    // swapping for the data becomes its source for the next pass
    [src, base] = [base, src];
  }

  return src;
};
