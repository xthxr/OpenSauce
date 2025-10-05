fun linearSearch(arr: IntArray, target: Int): Int {
    for (i in arr.indices) {   // loop through each index
        if (arr[i] == target) {
            return i           // return index if found
        }
    }
    return -1                  // return -1 if not found
}

fun main() {
    val arr = intArrayOf(1, 3, 5, 7, 9, 11, 13)
    val target = 7
    val result = linearSearch(arr, target)

    if (result != -1) {
        println("Element $target found at index $result")
    } else {
        println("Element $target not found in the array")
    }
}
