""" Given an integer array nums and an integer val, remove all occurrences of val in-place.
Return the number of elements that are not equal to val.

 Example:
    Input: nums = [3, 2, 2, 3], val = 3
    Output: 2, nums = [2, 2, _, _]
"""

class Solution(object):
    def removeElement(self, nums, val):
        j = 0
        for i in range(len(nums)):
            if nums[i] != val:
                nums[j] = nums[i]
                j+=1
        return j

# Example Usage
if __name__ == "__main__":
    nums = [3,2,2,4,3]
    val = 3
    obj = Solution()
    k = obj.removeElement(nums, val)
    print("k = ", k) # Prints 3
    print("Array after removal: ", nums[:k]) # Prints [2, 2, 4]