/*
Tower of Hanoi Algorithm

Time Complexity: O(2^n) where n is the number of disks
Space Complexity: O(n) due to recursive call stack

The Tower of Hanoi is a classic recursive problem where we need to move n disks
from source rod to destination rod using an auxiliary rod, following these rules:
1. Only one disk can be moved at a time
2. A disk can only be placed on top of a larger disk
3. Only the top disk of any rod can be moved
*/

#include <iostream>
using namespace std;

/**
 * Solves the Tower of Hanoi puzzle using recursion.
 * 
 * @param n Number of disks to move
 * @param source Name of the source rod
 * @param destination Name of the destination rod
 * @param auxiliary Name of the auxiliary rod
 */
void tower_of_hanoi(int n, char source, char destination, char auxiliary) {
    // Base case: If only 1 disk, move it directly
    if (n == 1) {
        cout << "Move disk 1 from " << source << " to " << destination << endl;
        return;
    }
    
    // Step 1: Move n-1 disks from source to auxiliary using destination
    tower_of_hanoi(n - 1, source, auxiliary, destination);
    
    // Step 2: Move the largest disk from source to destination
    cout << "Move disk " << n << " from " << source << " to " << destination << endl;
    
    // Step 3: Move n-1 disks from auxiliary to destination using source
    tower_of_hanoi(n - 1, auxiliary, destination, source);
}

// Main function - Example usage
int main() {
    int n = 3;  // Number of disks
    
    cout << "Tower of Hanoi Solution for " << n << " disks:\n" << endl;
    
    tower_of_hanoi(n, 'A', 'C', 'B');
    
    return 0;
}