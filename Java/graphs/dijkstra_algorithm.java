/**
 * Dijkstra's Algorithm - Shortest Path in Weighted Graph
 * 
 * Time Complexity: O((V + E) log V) using Priority Queue
 * Space Complexity: O(V) for distance array and priority queue
 * 
 * Dijkstra's algorithm finds the shortest path from a source vertex to all other
 * vertices in a weighted graph with non-negative edge weights. It uses a greedy
 * approach and is optimal for single-source shortest path problems.
 */

import java.util.*;

class DijkstraAlgorithm {
    
    /**
     * Represents a graph node with its distance from source
     */
    static class Node implements Comparable<Node> {
        int vertex;
        int distance;
        
        Node(int vertex, int distance) {
            this.vertex = vertex;
            this.distance = distance;
        }
        
        @Override
        public int compareTo(Node other) {
            return Integer.compare(this.distance, other.distance);
        }
    }
    
    /**
     * Find shortest distances from source to all vertices using Dijkstra's algorithm
     * 
     * @param graph Adjacency list representation of the graph
     * @param source Source vertex
     * @return Array of shortest distances from source to each vertex
     */
    public static int[] dijkstra(List<List<int[]>> graph, int source) {
        int n = graph.size();
        int[] distances = new int[n];
        boolean[] visited = new boolean[n];
        
        // Initialize distances to infinity
        Arrays.fill(distances, Integer.MAX_VALUE);
        distances[source] = 0;
        
        // Priority queue to store vertices with their distances
        PriorityQueue<Node> pq = new PriorityQueue<>();
        pq.offer(new Node(source, 0));
        
        while (!pq.isEmpty()) {
            Node current = pq.poll();
            int u = current.vertex;
            
            // Skip if already visited
            if (visited[u]) {
                continue;
            }
            
            visited[u] = true;
            
            // Check all neighbors of current vertex
            for (int[] edge : graph.get(u)) {
                int v = edge[0]; // destination vertex
                int weight = edge[1]; // edge weight
                
                // Relaxation step
                if (!visited[v] && distances[u] + weight < distances[v]) {
                    distances[v] = distances[u] + weight;
                    pq.offer(new Node(v, distances[v]));
                }
            }
        }
        
        return distances;
    }
    
    /**
     * Find shortest path from source to destination
     * 
     * @param graph Adjacency list representation of the graph
     * @param source Source vertex
     * @param destination Destination vertex
     * @return List representing the shortest path from source to destination
     */
    public static List<Integer> dijkstraPath(List<List<int[]>> graph, int source, int destination) {
        int n = graph.size();
        int[] distances = new int[n];
        int[] parent = new int[n];
        boolean[] visited = new boolean[n];
        
        Arrays.fill(distances, Integer.MAX_VALUE);
        Arrays.fill(parent, -1);
        distances[source] = 0;
        
        PriorityQueue<Node> pq = new PriorityQueue<>();
        pq.offer(new Node(source, 0));
        
        while (!pq.isEmpty()) {
            Node current = pq.poll();
            int u = current.vertex;
            
            if (visited[u]) {
                continue;
            }
            
            visited[u] = true;
            
            // Early termination if destination is reached
            if (u == destination) {
                break;
            }
            
            for (int[] edge : graph.get(u)) {
                int v = edge[0];
                int weight = edge[1];
                
                if (!visited[v] && distances[u] + weight < distances[v]) {
                    distances[v] = distances[u] + weight;
                    parent[v] = u;
                    pq.offer(new Node(v, distances[v]));
                }
            }
        }
        
        // Reconstruct path
        List<Integer> path = new ArrayList<>();
        if (distances[destination] == Integer.MAX_VALUE) {
            return path; // No path exists
        }
        
        int current = destination;
        while (current != -1) {
            path.add(current);
            current = parent[current];
        }
        
        Collections.reverse(path);
        return path;
    }
    
    /**
     * Create a sample graph for testing
     * 
     * @return Adjacency list representation of a sample graph
     */
    public static List<List<int[]>> createSampleGraph() {
        List<List<int[]>> graph = new ArrayList<>();
        
        // Create 6 vertices
        for (int i = 0; i < 6; i++) {
            graph.add(new ArrayList<>());
        }
        
        // Add edges (vertex, weight)
        graph.get(0).add(new int[]{1, 4});
        graph.get(0).add(new int[]{2, 2});
        
        graph.get(1).add(new int[]{2, 1});
        graph.get(1).add(new int[]{3, 5});
        
        graph.get(2).add(new int[]{3, 8});
        graph.get(2).add(new int[]{4, 10});
        
        graph.get(3).add(new int[]{4, 2});
        graph.get(3).add(new int[]{5, 6});
        
        graph.get(4).add(new int[]{5, 3});
        
        return graph;
    }
    
    /**
     * Print the shortest distances from source to all vertices
     * 
     * @param distances Array of shortest distances
     * @param source Source vertex
     */
    public static void printDistances(int[] distances, int source) {
        System.out.println("Shortest distances from vertex " + source + ":");
        for (int i = 0; i < distances.length; i++) {
            if (distances[i] == Integer.MAX_VALUE) {
                System.out.println("Vertex " + i + ": No path");
            } else {
                System.out.println("Vertex " + i + ": " + distances[i]);
            }
        }
    }
    
    /**
     * Print the shortest path from source to destination
     * 
     * @param path List representing the shortest path
     * @param source Source vertex
     * @param destination Destination vertex
     */
    public static void printPath(List<Integer> path, int source, int destination) {
        if (path.isEmpty()) {
            System.out.println("No path exists from " + source + " to " + destination);
        } else {
            System.out.print("Shortest path from " + source + " to " + destination + ": ");
            for (int i = 0; i < path.size(); i++) {
                System.out.print(path.get(i));
                if (i < path.size() - 1) {
                    System.out.print(" -> ");
                }
            }
            System.out.println();
        }
    }
    
    /**
     * Main method with test cases
     */
    public static void main(String[] args) {
        System.out.println("Dijkstra's Algorithm Test Cases");
        System.out.println("===============================");
        
        // Create sample graph
        List<List<int[]>> graph = createSampleGraph();
        
        // Test shortest distances from vertex 0
        System.out.println("\nTest 1: Shortest distances from vertex 0");
        int[] distances = dijkstra(graph, 0);
        printDistances(distances, 0);
        
        // Test shortest path from 0 to 5
        System.out.println("\nTest 2: Shortest path from 0 to 5");
        List<Integer> path = dijkstraPath(graph, 0, 5);
        printPath(path, 0, 5);
        
        // Test shortest path from 0 to 3
        System.out.println("\nTest 3: Shortest path from 0 to 3");
        path = dijkstraPath(graph, 0, 3);
        printPath(path, 0, 3);
        
        // Test shortest path from 2 to 5
        System.out.println("\nTest 4: Shortest path from 2 to 5");
        path = dijkstraPath(graph, 2, 5);
        printPath(path, 2, 5);
        
        // Test with different source
        System.out.println("\nTest 5: Shortest distances from vertex 2");
        distances = dijkstra(graph, 2);
        printDistances(distances, 2);
        
        // Performance test with larger graph
        System.out.println("\nPerformance Test:");
        long startTime = System.nanoTime();
        distances = dijkstra(graph, 0);
        long endTime = System.nanoTime();
        System.out.println("Time taken: " + (endTime - startTime) / 1000 + " microseconds");
    }
}
