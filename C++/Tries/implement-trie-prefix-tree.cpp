/*Problem: Leetcode 208. Implement Trie (Prefix Tree)

A trie (pronounced as "try") or prefix tree is a tree data structure used to efficiently store and retrieve keys in a dataset of strings. There are various applications of this data structure, such as autocomplete and spellchecker.

Implement the Trie class:

    Trie() Initializes the trie object.
    void insert(String word) Inserts the string word into the trie.
    boolean search(String word) Returns true if the string word is in the trie (i.e., was inserted before), and false otherwise.
    boolean startsWith(String prefix) Returns true if there is a previously inserted string word that has the prefix prefix, and false otherwise.

*/
#include <bits/stdc++.h>
using namespace std;


struct Node{
    Node *link[26];
    bool flag=false;

    bool ispresent(int i){
        return (link[i]!=NULL);
    }

    void putchar(int i, Node *node){
        link[i]=node;
    }

    Node* get(int i){
        return link[i];
    }

    void setend(){
        flag=true;
    }

    bool isend(){
        return flag;
    }
};

class Trie {
private:
    Node *root;
public:
    Trie() {
        root= new Node();
    }
    
    void insert(string word) {
        int n=word.length();
        Node *node= root;
        for(int i=0;i<n;i++){
            if(!node->ispresent(word[i]-'a')){
                node->putchar(word[i]-'a', new Node());
            }
            node=node->get(word[i]-'a');
        }
        node->setend();
    }
    
    bool search(string word) {
        int n=word.length();
        Node *node= root;
        for(int i=0;i<n;i++){
            if(!node->ispresent(word[i]-'a'))
                return false;
            node=node->get(word[i]-'a');
        }
        return node->isend();
    }
    
    bool startsWith(string prefix) {
        int n=prefix.length();
        Node *node= root;
        for(int i=0;i<n;i++){
            if(!node->ispresent(prefix[i]-'a'))
                return false;
            node=node->get(prefix[i]-'a');
        }
        return true;
    }
};
