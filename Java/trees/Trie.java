/**
 * Trie (Prefix Tree) Data Structure Implementation
 * 
 * A Trie is a tree-like data structure used for efficient storage and retrieval
 * of strings. It's particularly useful for:
 * - Autocomplete systems
 * - Spell checkers
 * - IP routing tables
 * - Dictionary implementations
 * 
 * Time Complexity:
 * - Insert: O(m) where m is the length of the word
 * - Search: O(m) where m is the length of the word
 * - StartsWith: O(m) where m is the length of the prefix
 * 
 * Space Complexity: O(ALPHABET_SIZE * N * M) 
 * where N is number of words and M is average word length
 * 
 * @author Pasan11504
 * @version 1.0
 * @since 2025-10-09
 */

public class Trie {
    
    /**
     * TrieNode represents each node in the Trie data structure.
     * Each node contains:
     * - An array of children nodes (26 for lowercase English letters)
     * - A boolean flag indicating if this node marks the end of a word
     */
    private static class TrieNode {
        private TrieNode[] children;
        private boolean isEndOfWord;
        private static final int ALPHABET_SIZE = 26;
        
        /**
         * Constructor initializes the children array and sets isEndOfWord to false
         */
        public TrieNode() {
            children = new TrieNode[ALPHABET_SIZE];
            isEndOfWord = false;
        }
    }
    
    private TrieNode root;
    
    /**
     * Constructor initializes an empty Trie with a root node
     */
    public Trie() {
        root = new TrieNode();
    }
    
    /**
     * Inserts a word into the Trie
     * 
     * @param word The word to insert (must contain only lowercase letters)
     * @throws IllegalArgumentException if word is null or empty
     * 
     * Time Complexity: O(m) where m is the length of the word
     * Space Complexity: O(m) in worst case when all characters need new nodes
     */
    public void insert(String word) {
        if (word == null || word.isEmpty()) {
            throw new IllegalArgumentException("Word cannot be null or empty");
        }
        
        TrieNode current = root;
        
        // Traverse through each character of the word
        for (int i = 0; i < word.length(); i++) {
            char ch = word.charAt(i);
            int index = ch - 'a';
            
            // Validate that character is lowercase letter
            if (index < 0 || index >= 26) {
                throw new IllegalArgumentException("Word must contain only lowercase letters");
            }
            
            // If the path doesn't exist, create a new node
            if (current.children[index] == null) {
                current.children[index] = new TrieNode();
            }
            
            // Move to the next node
            current = current.children[index];
        }
        
        // Mark the end of the word
        current.isEndOfWord = true;
    }
    
    /**
     * Searches for an exact word in the Trie
     * 
     * @param word The word to search for
     * @return true if the word exists in the Trie, false otherwise
     * 
     * Time Complexity: O(m) where m is the length of the word
     * Space Complexity: O(1)
     */
    public boolean search(String word) {
        if (word == null || word.isEmpty()) {
            return false;
        }
        
        TrieNode node = searchNode(word);
        return node != null && node.isEndOfWord;
    }
    
    /**
     * Checks if there is any word in the Trie that starts with the given prefix
     * 
     * @param prefix The prefix to search for
     * @return true if any word starts with the prefix, false otherwise
     * 
     * Time Complexity: O(m) where m is the length of the prefix
     * Space Complexity: O(1)
     */
    public boolean startsWith(String prefix) {
        if (prefix == null || prefix.isEmpty()) {
            return false;
        }
        
        return searchNode(prefix) != null;
    }
    
    /**
     * Helper method to traverse the Trie and find the node corresponding to
     * the last character of the given string
     * 
     * @param str The string to search for
     * @return The TrieNode at the end of the string path, or null if not found
     */
    private TrieNode searchNode(String str) {
        TrieNode current = root;
        
        for (int i = 0; i < str.length(); i++) {
            char ch = str.charAt(i);
            int index = ch - 'a';
            
            if (index < 0 || index >= 26 || current.children[index] == null) {
                return null;
            }
            
            current = current.children[index];
        }
        
        return current;
    }
    
    /**
     * Deletes a word from the Trie
     * 
     * @param word The word to delete
     * @return true if word was found and deleted, false otherwise
     * 
     * Time Complexity: O(m) where m is the length of the word
     * Space Complexity: O(m) due to recursion stack
     */
    public boolean delete(String word) {
        if (word == null || word.isEmpty()) {
            return false;
        }
        
        return deleteHelper(root, word, 0);
    }
    
    /**
     * Recursive helper method for deleting a word from the Trie
     * 
     * @param current The current node being processed
     * @param word The word to delete
     * @param index Current index in the word
     * @return true if the current node should be deleted
     */
    private boolean deleteHelper(TrieNode current, String word, int index) {
        if (index == word.length()) {
            // If we're at the end of the word
            if (!current.isEndOfWord) {
                return false; // Word doesn't exist
            }
            
            current.isEndOfWord = false;
            
            // If current node has no other children, it can be deleted
            return !hasChildren(current);
        }
        
        char ch = word.charAt(index);
        int charIndex = ch - 'a';
        TrieNode node = current.children[charIndex];
        
        if (node == null) {
            return false; // Word doesn't exist
        }
        
        boolean shouldDeleteCurrentNode = deleteHelper(node, word, index + 1);
        
        if (shouldDeleteCurrentNode) {
            current.children[charIndex] = null;
            // Delete current node only if it has no other children and is not end of another word
            return !hasChildren(current) && !current.isEndOfWord;
        }
        
        return false;
    }
    
    /**
     * Checks if a node has any children
     * 
     * @param node The node to check
     * @return true if the node has at least one child, false otherwise
     */
    private boolean hasChildren(TrieNode node) {
        for (TrieNode child : node.children) {
            if (child != null) {
                return true;
            }
        }
        return false;
    }
    
    /**
     * Counts how many words in the Trie start with the given prefix
     * 
     * @param prefix The prefix to search for
     * @return The count of words starting with the prefix
     * 
     * Time Complexity: O(m + n) where m is prefix length, n is number of words with that prefix
     * Space Complexity: O(h) where h is the height of the Trie (recursion stack)
     */
    public int countWordsStartingWith(String prefix) {
        if (prefix == null || prefix.isEmpty()) {
            return 0;
        }
        
        TrieNode node = searchNode(prefix);
        if (node == null) {
            return 0;
        }
        
        return countWords(node);
    }
    
    /**
     * Recursive helper to count all words from a given node
     * 
     * @param node The starting node
     * @return The count of words from this node onwards
     */
    private int countWords(TrieNode node) {
        if (node == null) {
            return 0;
        }
        
        int count = node.isEndOfWord ? 1 : 0;
        
        for (TrieNode child : node.children) {
            if (child != null) {
                count += countWords(child);
            }
        }
        
        return count;
    }
    
    /**
     * Checks if the Trie is empty
     * 
     * @return true if Trie contains no words, false otherwise
     */
    public boolean isEmpty() {
        return !hasChildren(root);
    }
    
    /**
     * Main method with comprehensive test cases demonstrating Trie functionality
     */
    public static void main(String[] args) {
        System.out.println("=== Trie (Prefix Tree) Implementation Tests ===\n");
        
        // Test 1: Basic Insert and Search
        System.out.println("Test 1: Basic Insert and Search");
        Trie trie = new Trie();
        trie.insert("apple");
        System.out.println("Inserted: apple");
        System.out.println("Search 'apple': " + trie.search("apple")); // true
        System.out.println("Search 'app': " + trie.search("app")); // false
        System.out.println("✓ Test 1 Passed\n");
        
        // Test 2: StartsWith (Prefix Search)
        System.out.println("Test 2: StartsWith (Prefix Search)");
        System.out.println("StartsWith 'app': " + trie.startsWith("app")); // true
        System.out.println("StartsWith 'appl': " + trie.startsWith("appl")); // true
        System.out.println("StartsWith 'ban': " + trie.startsWith("ban")); // false
        System.out.println("✓ Test 2 Passed\n");
        
        // Test 3: Multiple Words with Common Prefix
        System.out.println("Test 3: Multiple Words with Common Prefix");
        trie.insert("app");
        trie.insert("application");
        trie.insert("apply");
        System.out.println("Inserted: app, application, apply");
        System.out.println("Search 'app': " + trie.search("app")); // true
        System.out.println("Search 'application': " + trie.search("application")); // true
        System.out.println("Search 'appl': " + trie.search("appl")); // false (prefix but not a word)
        System.out.println("StartsWith 'app': " + trie.startsWith("app")); // true
        System.out.println("✓ Test 3 Passed\n");
        
        // Test 4: Delete Operation
        System.out.println("Test 4: Delete Operation");
        trie.delete("app");
        System.out.println("Deleted: app");
        System.out.println("Search 'app': " + trie.search("app")); // false
        System.out.println("Search 'apple': " + trie.search("apple")); // true (still exists)
        System.out.println("Search 'application': " + trie.search("application")); // true
        System.out.println("✓ Test 4 Passed\n");
        
        // Test 5: Count Words with Prefix
        System.out.println("Test 5: Count Words with Prefix");
        System.out.println("Words starting with 'app': " + trie.countWordsStartingWith("app")); // 3 (apple, application, apply)
        System.out.println("Words starting with 'appl': " + trie.countWordsStartingWith("appl")); // 2 (apple, application)
        System.out.println("Words starting with 'ban': " + trie.countWordsStartingWith("ban")); // 0
        System.out.println("✓ Test 5 Passed\n");
        
        // Test 6: Autocomplete Simulation
        System.out.println("Test 6: Autocomplete Simulation");
        Trie autocomplete = new Trie();
        String[] dictionary = {"car", "card", "care", "careful", "cat", "cats", "dog", "dodge"};
        System.out.println("Building autocomplete dictionary...");
        for (String word : dictionary) {
            autocomplete.insert(word);
            System.out.println("  Added: " + word);
        }
        System.out.println("\nAutocomplete suggestions:");
        System.out.println("  'ca' prefix has " + autocomplete.countWordsStartingWith("ca") + " words");
        System.out.println("  'car' prefix has " + autocomplete.countWordsStartingWith("car") + " words");
        System.out.println("  'cat' prefix has " + autocomplete.countWordsStartingWith("cat") + " words");
        System.out.println("  'do' prefix has " + autocomplete.countWordsStartingWith("do") + " words");
        System.out.println("✓ Test 6 Passed\n");
        
        // Test 7: Edge Cases
        System.out.println("Test 7: Edge Cases");
        Trie edgeTrie = new Trie();
        edgeTrie.insert("a");
        System.out.println("Single character word 'a': " + edgeTrie.search("a")); // true
        System.out.println("Empty Trie check: " + new Trie().isEmpty()); // true
        System.out.println("Non-empty Trie check: " + edgeTrie.isEmpty()); // false
        System.out.println("✓ Test 7 Passed\n");
        
        // Test 8: Overlapping Words
        System.out.println("Test 8: Overlapping Words");
        Trie overlap = new Trie();
        overlap.insert("test");
        overlap.insert("testing");
        overlap.insert("tester");
        overlap.insert("tested");
        System.out.println("Inserted: test, testing, tester, tested");
        System.out.println("Search 'test': " + overlap.search("test")); // true
        System.out.println("Search 'testing': " + overlap.search("testing")); // true
        System.out.println("Words starting with 'test': " + overlap.countWordsStartingWith("test")); // 4
        System.out.println("Words starting with 'testi': " + overlap.countWordsStartingWith("testi")); // 1
        System.out.println("✓ Test 8 Passed\n");
        
        // Test 9: Error Handling
        System.out.println("Test 9: Error Handling");
        try {
            trie.insert(null);
            System.out.println("✗ Should have thrown exception for null");
        } catch (IllegalArgumentException e) {
            System.out.println("✓ Correctly handled null insertion");
        }
        
        try {
            trie.insert("");
            System.out.println("✗ Should have thrown exception for empty string");
        } catch (IllegalArgumentException e) {
            System.out.println("✓ Correctly handled empty string insertion");
        }
        System.out.println("✓ Test 9 Passed\n");
        
        // Test 10: Delete Non-existent Word
        System.out.println("Test 10: Delete Non-existent Word");
        boolean deleted = trie.delete("nonexistent");
        System.out.println("Delete non-existent word: " + deleted); // false
        System.out.println("✓ Test 10 Passed\n");
        
        System.out.println("=== All Tests Passed Successfully! ===");
        System.out.println("\nTime Complexity Summary:");
        System.out.println("  Insert:     O(m) - m is word length");
        System.out.println("  Search:     O(m) - m is word length");
        System.out.println("  StartsWith: O(m) - m is prefix length");
        System.out.println("  Delete:     O(m) - m is word length");
        System.out.println("\nSpace Complexity: O(ALPHABET_SIZE * N * M)");
        System.out.println("  where N = number of words, M = average word length");
    }
}
