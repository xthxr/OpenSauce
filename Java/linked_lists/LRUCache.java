import java.io.*;
import java.util.*;

// LRU Cache implementation using Doubly Linked List and HashMap
// The idea is to maintain a doubly linked list to keep track of the order of usage
// and a hashmap to store the references of the nodes in the linked list for O(1) access.
//Time Complexity: O(1) for both get and put operations
//Space Complexity: O(capacity) for storing the cache elements
 
//Node class for doubly linked list
class Node{
    int data;
    Node next;
    Node prev;
    Node(int val){
        this.data=val; 
    }
}

//LRU class to implement LRU Cache
class LRU{
    HashMap<Integer,Node> hm=new HashMap<>(); // hashmap to store the references of nodes 
    Node lru=new Node(-1); // least recently used node
    Node mru=new Node(-1); // most recently used node
    int k; // capacity of the cache
    LRU(int size){
        k=size;
        lru.next=mru;
        mru.prev=lru;
    }
    void insert(int x){
        if(hm.containsKey(x)){ // if the element is already present in the cache
            Node t=hm.get(x); // get the node from hashmap
            remove(t);  // remove the node from its current position
            insertmru(t);  // insert the node at the most recently used position
        }
        else{
            Node t=new Node(x); 
            hm.put(x,t);        
            insertmru(t);       

            if(hm.size() > k){
                Node p=lru.next;
                remove(p);
                hm.remove(p.data);
            }
        }
    }
    void remove(Node t){ // remove the node from its current position
        t.prev.next=t.next;
        t.next.prev=t.prev;
    }
    void insertmru(Node t){ // insert the node at the most recently used position
        t.prev=mru.prev;
        t.next=mru;
        mru.prev.next=t;
        mru.prev=t;
    }
    void printcache(){ // print the current state of the cache from LRU to MRU
        Node cur=lru.next;
        while(cur!=mru){
            System.out.print(cur.data+" ");
            cur=cur.next;
        }
        System.out.println();
    }
}
public class LRUCache {

    public static void main(String[] args) {
        /* Enter your code here. Read input from STDIN. Print output to STDOUT. Your class should be named Main. */
        Scanner sc=new Scanner(System.in);
        System.out.println("Enter the number of test cases:");
        int t=sc.nextInt();
        while(t-- > 0){
            System.out.println("Enter the number of operations and capacity of the cache:");
            int n=sc.nextInt();
            int k=sc.nextInt(); // capacity of the cache
            System.out.println("Enter the elements to be accessed:");
            int[] ar=new int[n];
            for(int i=0;i<n;i++){
                ar[i]=sc.nextInt();
            }
            LRU l=new LRU(k);
            for(int i=0;i<n;i++){
                l.insert(ar[i]); // access the element
            }
            System.out.println("Current state of the cache from LRU to MRU:");
            l.printcache();
        }
    }
}
