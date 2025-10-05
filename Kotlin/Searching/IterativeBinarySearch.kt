fun binarySearch(arr: IntArray, target: Int): Int {
    var left = 0
    var right = arr.size - 1

    while (left <= right) {
        val mid = left + (right - left) / 2  // avoid overflow
        when {
            arr[mid] == target -> return mid  // found
            arr[mid] < target -> left = mid + 1
            else -> right = mid - 1
        }
    }
    return -1  // not found
}

fun main() {
    val numbers = intArrayOf(1, 3, 5, 7, 9, 11)
    val target = 7
    val result = binarySearch(numbers, target)

    if (result != -1)
        println("Element found at index $result")
    else
        println("Element not found")
}
