/*
 * Title: Dijkstra’s Algorithm using Priority Queue (Optimized)
 * Description: Implementation of Dijkstra's algorithm to find the shortest
 *              paths from a source vertex to all vertices in a weighted graph.
 * 
 * Approach:
 * Uses a Min-Heap (PriorityQueue) to optimize edge relaxation operations,
 * reducing time complexity to O((V + E) log V).
 * 
 * Example:
 * Input:
 * V = 5, E = 6
 * Edges:
 * 0 1 2
 * 0 2 4
 * 1 2 1
 * 1 3 7
 * 2 4 3
 * 3 4 1
 * Source: 0
 *
 * Output:
 * Shortest distances from node 0: [0, 2, 3, 9, 6]
 *
 * Compilation:
 *   javac DijkstraAlgorithm.java
 * Execution:
 *   java DijkstraAlgorithm
 *
 * Author: Your Name
 */

import java.util.*;

public class Dijkstra {

    // Class to represent an edge with destination vertex and weight
    static class Edge {
        int destination;
        int weight;

        Edge(int destination, int weight) {
            this.destination = destination;
            this.weight = weight;
        }
    }

    // Class to represent a pair of vertex and current distance for PriorityQueue
    static class Node implements Comparable<Node> {
        int vertex;
        int distance;

        Node(int vertex, int distance) {
            this.vertex = vertex;
            this.distance = distance;
        }

        @Override
        public int compareTo(Node other) {
            return this.distance - other.distance;
        }
    }

    // Dijkstra algorithm implementation
    public static void dijkstra(List<List<Edge>> graph, int source, int V) {
        int[] dist = new int[V];
        Arrays.fill(dist, Integer.MAX_VALUE);
        dist[source] = 0;

        PriorityQueue<Node> pq = new PriorityQueue<>();
        pq.add(new Node(source, 0));

        while (!pq.isEmpty()) {
            Node current = pq.poll();
            int u = current.vertex;

            for (Edge edge : graph.get(u)) {
                int v = edge.destination;
                int weight = edge.weight;

                // Relaxation step
                if (dist[u] + weight < dist[v]) {
                    dist[v] = dist[u] + weight;
                    pq.add(new Node(v, dist[v]));
                }
            }
        }

        // Print shortest distances
        System.out.println("Shortest distances from node " + source + ":");
        System.out.println(Arrays.toString(dist));
    }

    // Driver Code
    public static void main(String[] args) {
        int V = 5;
        List<List<Edge>> graph = new ArrayList<>();

        for (int i = 0; i < V; i++) {
            graph.add(new ArrayList<>());
        }

        // Adding edges (u -> v, weight)
        graph.get(0).add(new Edge(1, 2));
        graph.get(0).add(new Edge(2, 4));
        graph.get(1).add(new Edge(2, 1));
        graph.get(1).add(new Edge(3, 7));
        graph.get(2).add(new Edge(4, 3));
        graph.get(3).add(new Edge(4, 1));

        int source = 0;
        dijkstra(graph, source, V);
    }
}
