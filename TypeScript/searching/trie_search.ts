/**
 * @fileoverview Implements a Trie (Prefix Tree) data structure for efficient word storage and lookup.
 * @author Dimaswahyu-official
 */

/**
 * A Trie Node represents each character in the Trie.
 * Each node contains a map of children nodes and a flag indicating if it's the end of a word.
 */
class TrieNode {
    children: Map<string, TrieNode>;
    isEndOfWord: boolean;

    constructor() {
        this.children = new Map();
        this.isEndOfWord = false;
    }
}

/**
 * The Trie (Prefix Tree) class allows efficient insertion and searching of words or prefixes.
 *
 * Trie Search is an algorithm commonly used for fast word lookups, autocomplete systems,
 * spell-checkers, and prefix-based search operations. It stores characters in a tree-like structure
 * where each node represents a character in the string.
 *
 * @methods
 * - insert(word: string): void — Inserts a word into the Trie.
 * - search(word: string): boolean — Returns true if the exact word exists in the Trie.
 * - startsWith(prefix: string): boolean — Returns true if there is any word in the Trie starting with the given prefix.
 *
 * @complexity
 * Time Complexity:
 *   - Insert: O(L), where L = length of the word.
 *   - Search: O(L)
 *   - StartsWith: O(P), where P = length of the prefix.
 * Space Complexity: O(AL), where A = size of alphabet and L = total characters stored.
 */
class Trie {
    private root: TrieNode;

    constructor() {
        this.root = new TrieNode();
    }

    /**
     * Inserts a word into the Trie.
     * @param word The word to insert.
     */
    insert(word: string): void {
        let node = this.root;

        for (const char of word) {
            if (!node.children.has(char)) {
                node.children.set(char, new TrieNode());
            }
            node = node.children.get(char)!;
        }

        node.isEndOfWord = true;
    }

    /**
     * Searches for an exact word in the Trie.
     * @param word The word to search for.
     * @returns True if the word exists, otherwise false.
     */
    search(word: string): boolean {
        let node = this.root;

        for (const char of word) {
            if (!node.children.has(char)) return false;
            node = node.children.get(char)!;
        }

        return node.isEndOfWord;
    }

    /**
     * Checks if there exists any word in the Trie that starts with the given prefix.
     * @param prefix The prefix to check.
     * @returns True if the prefix exists in any word, otherwise false.
     */
    startsWith(prefix: string): boolean {
        let node = this.root;

        for (const char of prefix) {
            if (!node.children.has(char)) return false;
            node = node.children.get(char)!;
        }

        return true;
    }
}

// --- Example Usage ---

const trie = new Trie();

// 1. Insert words
trie.insert("apple");
trie.insert("app");
trie.insert("application");
trie.insert("banana");

// 2. Search for existing words
console.log(`Search "apple": ${trie.search("apple")} (Expected: true)`); // true
console.log(`Search "app": ${trie.search("app")} (Expected: true)`); // true

// 3. Search for non-existing word
console.log(`Search "apples": ${trie.search("apples")} (Expected: false)`); // false

// 4. Check prefix
console.log(`Starts with "ap": ${trie.startsWith("ap")} (Expected: true)`); // true
console.log(`Starts with "ban": ${trie.startsWith("ban")} (Expected: true)`); // true
console.log(`Starts with "bat": ${trie.startsWith("bat")} (Expected: false)`); // false
