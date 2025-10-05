def linear_search(arr, target):
    """
    Function to perform linear search.
    Returns the index of target element if found, else -1.
    """
    for i in range(len(arr)):
        if arr[i] == target:
            return i  # Return the index where target is found
    return -1  # If target not found


# --- Example usage ---
numbers = [10, 25, 30, 45, 50, 60]
key = int(input("Enter the number to search: "))

result = linear_search(numbers, key)

if result != -1:
    print(f"Element found at index {result}")
else:
    print("Element not found in the list.")
