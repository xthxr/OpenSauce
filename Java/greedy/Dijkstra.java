package Java.greedy;

/*
 * Djikstra's algorithm implementation
 * Time Complexity: O((V + E) log V) using a priority queue
 * Space Complexity: O(V) for storing distances and priority queue
 * Dijkstra's algorithm finds the shortest path from a source vertex to all other vertices in a weighted graph with non-negative weights.
 * It works by iteratively selecting the vertex with the smallest known distance, updating the distances to its neighbors, and repeating until all vertices have been processed.
 * 
 * 
 * Here, we use an adjacency matrix to represent the graph
 * Time complexity is O(V^2) in this case
 * Space complexity is O(V) for storing distances
 */

import java.util.Arrays;
import java.util.Set;
import java.util.HashSet;

public class Dijkstra {
    // Graph represented as an adjacency matrix
    private int graph[][];
    private int V;

    public Dijkstra(int graph[][]) {
        this.graph = graph;
        this.V = graph.length;
    }

    public int[] minDistance(int start) {
        int distances[] = new int[V];

        // Initialize distances to all vertices as infinite and distance to source as 0
        Arrays.fill(distances, Integer.MAX_VALUE);
        distances[start] = 0;

        Set<Integer> visited = new HashSet<>(); // Track visited vertices

        for (int i = 0; i < V - 1; i++) {
            int minDistance = Integer.MAX_VALUE;
            int minIndex = -1;
            
            // Find the vertex with the minimum distance
            for (int v = 0; v < V; v++) {
                if (!visited.contains(v) && distances[v] <= minDistance) {
                    minDistance = distances[v];
                    minIndex = v;
                }
            }
            // if no unvisited vertex is found, break
            if (minIndex == -1) {
                break;
            }

            // Mark the selected vertex as visited
            visited.add(minIndex);

            // Update distances to neighboring vertices
            for (int v = 0; v < V; v++) {
                if (graph[minIndex][v] != 0 && !visited.contains(v)) {
                    int newDist = distances[minIndex] + graph[minIndex][v];
                    if (newDist < distances[v]) {
                        distances[v] = newDist;
                    }
                }
            }
        }
        return distances;
    }

    public static void main(String args[]) {
        // Example graph (adjacency matrix)
        // 0 represents no edge
        int graph[][] = new int[][] { 
            { 0, 7, 9, 0, 0, 14 },
            { 7, 0, 10, 15, 0, 0 },
            { 9, 10, 0, 11, 0, 2 },
            { 0, 15, 11, 0, 6, 0 },
            { 0, 0, 0, 6, 0, 9 },
            { 14, 0, 2, 0, 9, 0 } 
        };


        Dijkstra dijkstra = new Dijkstra(graph);
        int startVertex = 0;
        int[] distances = dijkstra.minDistance(startVertex);
        
        System.out.println("Vertex\tDistance from Source");
        for (int i = 0; i < distances.length; i++) {
            System.out.println(i + "\t" + distances[i]);
        }
    }
}

