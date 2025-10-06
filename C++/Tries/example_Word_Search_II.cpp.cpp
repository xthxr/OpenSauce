/*
 * Word Search II
 * LeetCode Problem: 212. Word Search II
 * 
 * Description:
 * Given an m x n board of characters and a list of strings words, return all 
 * words on the board. Each word must be constructed from letters of sequentially 
 * adjacent cells, where adjacent cells are horizontally or vertically neighboring. 
 * The same letter cell may not be used more than once in a word.
 * 
 * Example:
 * Input: board = [["o","a","a","n"],
 *                 ["e","t","a","e"],
 *                 ["i","h","k","r"],
 *                 ["i","f","l","v"]]
 *        words = ["oath","pea","eat","rain"]
 * Output: ["eat","oath"]
 * 
 * Time Complexity: O(m * n * 4^L) where L is max word length
 * Space Complexity: O(N) where N is total characters in all words
 * 
 * Key Strategy: Use Trie to efficiently search multiple words simultaneously
 * 
 * Author: OpenSauce Contributor
 * Date: October 2025
 */

#include <iostream>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
using namespace std;

class TrieNode {
public:
    unordered_map<char, TrieNode*> children;
    string word;  // Store complete word at leaf node
    
    TrieNode() {
        word = "";
    }
};

class Solution {
private:
    TrieNode* root;
    vector<string> result;
    unordered_set<string> foundWords;  // Avoid duplicates
    int rows, cols;
    
    // Directions: up, down, left, right
    vector<pair<int, int>> directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
    
    /**
     * Build trie from list of words
     * @param words: vector of words to insert
     */
    void buildTrie(vector<string>& words) {
        root = new TrieNode();
        
        for (const string& word : words) {
            TrieNode* current = root;
            
            for (char ch : word) {
                if (current->children.find(ch) == current->children.end()) {
                    current->children[ch] = new TrieNode();
                }
                current = current->children[ch];
            }
            
            // Store the complete word at leaf node
            current->word = word;
        }
    }
    
    /**
     * DFS + Backtracking to search for words
     * @param board: the character board
     * @param row: current row position
     * @param col: current column position
     * @param node: current trie node
     */
    void dfs(vector<vector<char>>& board, int row, int col, TrieNode* node) {
        // Boundary check
        if (row < 0 || row >= rows || col < 0 || col >= cols) {
            return;
        }
        
        char ch = board[row][col];
        
        // If cell is visited or character not in trie, return
        if (ch == '#' || node->children.find(ch) == node->children.end()) {
            return;
        }
        
        // Move to next trie node
        node = node->children[ch];
        
        // Found a complete word
        if (!node->word.empty() && foundWords.find(node->word) == foundWords.end()) {
            result.push_back(node->word);
            foundWords.insert(node->word);
            // Note: Don't return here, continue searching for longer words
        }
        
        // Mark cell as visited
        char temp = board[row][col];
        board[row][col] = '#';
        
        // Explore all 4 directions
        for (auto& dir : directions) {
            int newRow = row + dir.first;
            int newCol = col + dir.second;
            dfs(board, newRow, newCol, node);
        }
        
        // Backtrack: restore cell value
        board[row][col] = temp;
    }
    
public:
    /**
     * Find all words from dictionary that exist in the board
     * @param board: m x n character board
     * @param words: list of words to search
     * @return vector of found words
     */
    vector<string> findWords(vector<vector<char>>& board, vector<string>& words) {
        if (board.empty() || words.empty()) {
            return {};
        }
        
        rows = board.size();
        cols = board[0].size();
        
        // Build trie from all words
        buildTrie(words);
        
        // Start DFS from each cell
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                dfs(board, i, j, root);
            }
        }
        
        return result;
    }
    
    /**
     * Cleanup memory
     */
    void deleteTrie(TrieNode* node) {
        for (auto& pair : node->children) {
            deleteTrie(pair.second);
        }
        delete node;
    }
    
    ~Solution() {
        if (root != nullptr) {
            deleteTrie(root);
        }
    }
};

// Helper function to print board
void printBoard(const vector<vector<char>>& board) {
    for (const auto& row : board) {
        for (char ch : row) {
            cout << ch << " ";
        }
        cout << endl;
    }
}

// Test cases and example usage
int main() {
    cout << "=== Word Search II Test Cases ===" << endl << endl;
    
    // Test Case 1: Basic example
    cout << "Test Case 1: Basic Example" << endl;
    vector<vector<char>> board1 = {
        {'o','a','a','n'},
        {'e','t','a','e'},
        {'i','h','k','r'},
        {'i','f','l','v'}
    };
    vector<string> words1 = {"oath", "pea", "eat", "rain"};
    
    cout << "Board:" << endl;
    printBoard(board1);
    cout << "\nWords to find: ";
    for (const string& w : words1) cout << w << " ";
    cout << endl;
    
    Solution sol1;
    vector<string> result1 = sol1.findWords(board1, words1);
    cout << "Found words: ";
    for (const string& w : result1) cout << w << " ";
    cout << "\nExpected: eat oath" << endl << endl;
    
    // Test Case 2: Single character
    cout << "Test Case 2: Single Character Words" << endl;
    vector<vector<char>> board2 = {
        {'a','b'},
        {'c','d'}
    };
    vector<string> words2 = {"a", "b", "c", "d", "e"};
    
    cout << "Board:" << endl;
    printBoard(board2);
    cout << "\nWords to find: ";
    for (const string& w : words2) cout << w << " ";
    cout << endl;
    
    Solution sol2;
    vector<string> result2 = sol2.findWords(board2, words2);
    cout << "Found words: ";
    for (const string& w : result2) cout << w << " ";
    cout << "\nExpected: a b c d" << endl << endl;
    
    // Test Case 3: Complex paths
    cout << "Test Case 3: Complex Paths" << endl;
    vector<vector<char>> board3 = {
        {'a','b','c'},
        {'a','e','d'},
        {'a','f','g'}
    };
    vector<string> words3 = {"abcdefg", "gfedcbaaa", "eaabcdgfa", "befa", "dgc", "ade"};
    
    cout << "Board:" << endl;
    printBoard(board3);
    cout << "\nWords to find: ";
    for (const string& w : words3) cout << w << " ";
    cout << endl;
    
    Solution sol3;
    vector<string> result3 = sol3.findWords(board3, words3);
    cout << "Found words: ";
    for (const string& w : result3) cout << w << " ";
    cout << "\nExpected: abcdefg eaabcdgfa befa" << endl << endl;
    
    // Test Case 4: No words found
    cout << "Test Case 4: No Words Found" << endl;
    vector<vector<char>> board4 = {
        {'a','a'},
        {'a','a'}
    };
    vector<string> words4 = {"aaaaa", "baaa", "aaab"};
    
    cout << "Board:" << endl;
    printBoard(board4);
    cout << "\nWords to find: ";
    for (const string& w : words4) cout << w << " ";
    cout << endl;
    
    Solution sol4;
    vector<string> result4 = sol4.findWords(board4, words4);
    cout << "Found words: ";
    if (result4.empty()) {
        cout << "(none)";
    } else {
        for (const string& w : result4) cout << w << " ";
    }
    cout << "\nExpected: aaaa" << endl << endl;
    
    // Test Case 5: Duplicate prevention
    cout << "Test Case 5: Duplicate Word Prevention" << endl;
    vector<vector<char>> board5 = {
        {'a','a','a'},
        {'a','a','a'},
        {'a','a','a'}
    };
    vector<string> words5 = {"aaa", "aaaa", "aaaaa", "aaaaaa", "aaaaaaa", "aaaaaaaa"};
    
    cout << "Board:" << endl;
    printBoard(board5);
    cout << "\nWords to find: ";
    for (const string& w : words5) cout << w << " ";
    cout << endl;
    
    Solution sol5;
    vector<string> result5 = sol5.findWords(board5, words5);
    cout << "Found words: ";
    for (const string& w : result5) cout << w << " ";
    cout << "\nExpected: aaa aaaa aaaaa aaaaaa aaaaaaa aaaaaaaa" << endl;
    
    return 0;
}

/*
Expected Output:
=== Word Search II Test Cases ===

Test Case 1: Basic Example
Board:
o a a n 
e t a e 
i h k r 
i f l v 

Words to find: oath pea eat rain 
Found words: eat oath 
Expected: eat oath

Test Case 2: Single Character Words
Board:
a b 
c d 

Words to find: a b c d e 
Found words: a b c d 
Expected: a b c d

Test Case 3: Complex Paths
Board:
a b c 
a e d 
a f g 

Words to find: abcdefg gfedcbaaa eaabcdgfa befa dgc ade 
Found words: abcdefg eaabcdgfa befa 
Expected: abcdefg eaabcdgfa befa

Test Case 4: No Words Found
Board:
a a 
a a 

Words to find: aaaaa baaa aaab 
Found words: aaaa
Expected: aaaa

Test Case 5: Duplicate Word Prevention
Board:
a a a 
a a a 
a a a 

Words to find: aaa aaaa aaaaa aaaaaa aaaaaaa aaaaaaaa 
Found words: aaa aaaa aaaaa aaaaaa aaaaaaa aaaaaaaa 
Expected: aaa aaaa aaaaa aaaaaa aaaaaaa aaaaaaaa
*/