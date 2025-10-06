/*
 * Basic Trie (Prefix Tree) Implementation
 * 
 * Description:
 * A trie (pronounced as "try") or prefix tree is a tree data structure 
 * used to efficiently store and retrieve keys in a dataset of strings.
 * 
 * Operations:
 * - insert(word): Inserts a string word into the trie
 * - search(word): Returns true if the string word is in the trie
 * - startsWith(prefix): Returns true if there is a previously inserted string 
 *   that has the prefix
 * 
 * Time Complexity:
 * - insert: O(m) where m is the length of the word
 * - search: O(m) where m is the length of the word
 * - startsWith: O(m) where m is the length of the prefix
 * 
 * Space Complexity: O(ALPHABET_SIZE * N * M) where N is number of words 
 * and M is average length
 * 
 * Author: OpenSauce Contributor
 * Date: October 2025
 */

#include <iostream>
#include <string>
#include <unordered_map>
using namespace std;

class TrieNode {
public:
    unordered_map<char, TrieNode*> children;
    bool isEndOfWord;
    
    TrieNode() {
        isEndOfWord = false;
    }
};

class Trie {
private:
    TrieNode* root;
    
public:
    Trie() {
        root = new TrieNode();
    }
    
    /**
     * Inserts a word into the trie
     * @param word: string to insert
     */
    void insert(string word) {
        TrieNode* current = root;
        
        for (char ch : word) {
            // If character doesn't exist, create new node
            if (current->children.find(ch) == current->children.end()) {
                current->children[ch] = new TrieNode();
            }
            // Move to child node
            current = current->children[ch];
        }
        
        // Mark end of word
        current->isEndOfWord = true;
    }
    
    /**
     * Searches for a complete word in the trie
     * @param word: string to search
     * @return true if word exists, false otherwise
     */
    bool search(string word) {
        TrieNode* current = root;
        
        for (char ch : word) {
            // If character not found, word doesn't exist
            if (current->children.find(ch) == current->children.end()) {
                return false;
            }
            current = current->children[ch];
        }
        
        // Return true only if it's marked as end of word
        return current->isEndOfWord;
    }
    
    /**
     * Checks if there is any word in the trie that starts with given prefix
     * @param prefix: prefix to search
     * @return true if prefix exists, false otherwise
     */
    bool startsWith(string prefix) {
        TrieNode* current = root;
        
        for (char ch : prefix) {
            if (current->children.find(ch) == current->children.end()) {
                return false;
            }
            current = current->children[ch];
        }
        
        return true;
    }
    
    /**
     * Helper function to delete trie and free memory
     */
    void deleteTrie(TrieNode* node) {
        for (auto& pair : node->children) {
            deleteTrie(pair.second);
        }
        delete node;
    }
    
    ~Trie() {
        deleteTrie(root);
    }
};

// Test cases and example usage
int main() {
    cout << "=== Trie Implementation Test Cases ===" << endl << endl;
    
    // Test Case 1: Basic Operations
    cout << "Test Case 1: Basic Insert and Search" << endl;
    Trie trie1;
    trie1.insert("apple");
    cout << "Inserted: apple" << endl;
    cout << "Search 'apple': " << (trie1.search("apple") ? "Found" : "Not Found") << endl;
    cout << "Search 'app': " << (trie1.search("app") ? "Found" : "Not Found") << endl;
    cout << "StartsWith 'app': " << (trie1.startsWith("app") ? "True" : "False") << endl;
    
    trie1.insert("app");
    cout << "Inserted: app" << endl;
    cout << "Search 'app': " << (trie1.search("app") ? "Found" : "Not Found") << endl;
    cout << endl;
    
    // Test Case 2: Multiple Words
    cout << "Test Case 2: Multiple Words" << endl;
    Trie trie2;
    string words[] = {"hello", "world", "hell", "heaven", "heavy"};
    
    cout << "Inserting words: ";
    for (string word : words) {
        trie2.insert(word);
        cout << word << " ";
    }
    cout << endl;
    
    cout << "Search 'hello': " << (trie2.search("hello") ? "Found" : "Not Found") << endl;
    cout << "Search 'hel': " << (trie2.search("hel") ? "Found" : "Not Found") << endl;
    cout << "StartsWith 'hea': " << (trie2.startsWith("hea") ? "True" : "False") << endl;
    cout << "StartsWith 'wor': " << (trie2.startsWith("wor") ? "True" : "False") << endl;
    cout << "Search 'heavy': " << (trie2.search("heavy") ? "Found" : "Not Found") << endl;
    cout << endl;
    
    // Test Case 3: Edge Cases
    cout << "Test Case 3: Edge Cases" << endl;
    Trie trie3;
    trie3.insert("a");
    cout << "Inserted: a" << endl;
    cout << "Search 'a': " << (trie3.search("a") ? "Found" : "Not Found") << endl;
    cout << "Search '': " << (trie3.search("") ? "Found" : "Not Found") << endl;
    cout << "StartsWith '': " << (trie3.startsWith("") ? "True" : "False") << endl;
    
    return 0;
}

/*
Expected Output:
=== Trie Implementation Test Cases ===

Test Case 1: Basic Insert and Search
Inserted: apple
Search 'apple': Found
Search 'app': Not Found
StartsWith 'app': True
Inserted: app
Search 'app': Found

Test Case 2: Multiple Words
Inserting words: hello world hell heaven heavy 
Search 'hello': Found
Search 'hel': Not Found
StartsWith 'hea': True
StartsWith 'wor': True
Search 'heavy': Found

Test Case 3: Edge Cases
Inserted: a
Search 'a': Found
Search '': Not Found
StartsWith '': True
*/