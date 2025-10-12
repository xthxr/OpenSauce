/*You are given an image represented by an m x n grid of integers image, where image[i][j] represents the pixel value of the image. You are also given three integers sr, sc, and color. Your task is to perform a flood fill on the image starting from the pixel image[sr][sc].

To perform a flood fill:

Begin with the starting pixel and change its color to color.
Perform the same process for each pixel that is directly adjacent (pixels that share a side with the original pixel, either horizontally or vertically) and shares the same color as the starting pixel.
Keep repeating this process by checking neighboring pixels of the updated pixels and modifying their color if it matches the original color of the starting pixel.
The process stops when there are no more adjacent pixels of the original color to update.
Return the modified image after performing the flood fill.

Example 1:

Input: image = [[1,1,1],[1,1,0],[1,0,1]], sr = 1, sc = 1, color = 2

Output: [[2,2,2],[2,2,0],[2,0,1]]

Explanation:
From the center of the image with position (sr, sc) = (1, 1) (i.e., the red pixel), all pixels connected by a path of the same color as the starting pixel (i.e., the blue pixels) are colored with the new color.

Note the bottom corner is not colored 2, because it is not horizontally or vertically connected to the starting pixel.

Example 2:

Input: image = [[0,0,0],[0,0,0]], sr = 0, sc = 0, color = 0

Output: [[0,0,0],[0,0,0]]

Explanation:

The starting pixel is already colored with 0, which is the same as the target color. Therefore, no changes are made to the image.

*/

// using dfs approach
// Time Complexity: O(m*n) where m is the number of rows and n is the number of columns in the image
// Space Complexity: O(m*n) for the recursion stack in the worst case
#include <iostream>
#include <vector>
using namespace std;

// Helper function for Depth-First Search (DFS)
void dfs(vector<vector<int>> &image, int x,
         int y, int oldColor, int newColor)
{

    // Base case: check boundary conditions and color mismatch
    if (x < 0 || x >= image.size() ||
        y < 0 || y >= image[0].size() ||
        image[x][y] != oldColor)
    {
        // Backtrack if pixel is out of bounds or color doesn't match
        return;
    }

    // Update the color of the current pixel
    image[x][y] = newColor;

    // Recursively visit all 4 connected neighbors
    dfs(image, x + 1, y, oldColor, newColor);
    dfs(image, x - 1, y, oldColor, newColor);
    dfs(image, x, y + 1, oldColor, newColor);
    dfs(image, x, y - 1, oldColor, newColor);
}

// Main flood fill function
vector<vector<int>> floodFill(
    vector<vector<int>> &image, int sr,
    int sc, int newColor)
{

    // If the starting pixel already has the new color,
    // no changes are needed
    if (image[sr][sc] == newColor)
    {
        return image;
    }

    // Call DFS to start filling from the source pixel
    int oldColor = image[sr][sc]; // Store original color
    dfs(image, sr, sc, oldColor, newColor);

    return image; // Return the updated image
}

// Driver code to test the flood fill function
int main()
{
    // Input image (2D grid)
    vector<vector<int>> image = {
        {1, 1, 1, 0},
        {0, 1, 1, 1},
        {1, 0, 1, 1}};

    // Starting pixel (row, col)
    int sr = 1, sc = 2;

    // New color to apply
    int newColor = 2;

    // Perform flood fill and get the result
    vector<vector<int>> result = floodFill(image, sr, sc, newColor);

    // Print the updated image
    for (auto &row : result)
    {
        for (auto &pixel : row)
        {
            cout << pixel << " ";
        }
        cout << "\n";
    }
    return 0;
}

// bfs approach
// Time Complexity: O(m*n) where m is the number of rows and n is the number of columns in the image
// Space Complexity: O(m*n) for the queue in the worst case

#include <iostream>
#include <vector>
#include <queue>
using namespace std;

vector<vector<int>> floodFill(
    vector<vector<int>> &image, int sr,
    int sc, int newColor)
{

    // If the starting pixel already has the new color
    if (image[sr][sc] == newColor)
    {
        return image;
    }

    // Direction vectors for traversing 4 directions
    vector<pair<int, int>> directions = {
        {1, 0}, {-1, 0}, {0, 1}, {0, -1}};

    // Initialize the queue for BFS
    queue<pair<int, int>> q;
    int oldColor = image[sr][sc];
    q.push({sr, sc});

    // Change the color of the starting pixel
    image[sr][sc] = newColor;

    // Perform BFS
    while (!q.empty())
    {
        pair<int, int> front = q.front();
        int x = front.first, y = front.second;
        q.pop();

        // Traverse all 4 directions
        for (const pair<int, int> &direction : directions)
        {
            int nx = x + direction.first;
            int ny = y + direction.second;

            // Check boundary conditions and color match
            if (nx >= 0 && nx < image.size() &&
                ny >= 0 && ny < image[0].size() &&
                image[nx][ny] == oldColor)
            {

                // Change the color and enqueue
                image[nx][ny] = newColor;
                q.push({nx, ny});
            }
        }
    }

    return image;
}

int main()
{

    // Define the input 2D image (grid of pixel colors)
    vector<vector<int>> image = {
        {1, 1, 1, 0},
        {0, 1, 1, 1},
        {1, 0, 1, 1}};

    // Starting pixel coordinates (row = 1, column = 2)
    int sr = 1, sc = 2;

    // New color to apply to the connected region
    int newColor = 2;

    // Call the floodFill function to perform DFS/BFS fill from the
    // starting pixel
    vector<vector<int>> result = floodFill(image, sr, sc, newColor);

    // Print the updated image after flood fill
    for (auto &row : result)
    {
        for (auto &pixel : row)
        {

            // Print each pixel with a space
            cout << pixel << " ";
        }

        // Move to the next line after printing each row
        cout << "\n";
    }

    return 0;
}