import java.util.*;

/**
 * Dijkstra's Algorithm
 * 
 * Time Complexity: O((V + E) log V) with priority queue
 * Space Complexity: O(V) for distance array and priority queue
 * 
 * Dijkstra's algorithm finds the shortest path from a source vertex to all
 * other vertices in a weighted graph with non-negative edge weights.
 */
public class DijkstraAlgorithm {

  private int vertices;
  private List<Edge>[] adjacencyList;

  /**
   * Inner class to represent an edge with weight.
   */
  static class Edge {
    int destination;
    int weight;

    public Edge(int destination, int weight) {
      this.destination = destination;
      this.weight = weight;
    }
  }

  /**
   * Inner class to represent a node in the priority queue.
   */
  static class Node implements Comparable<Node> {
    int vertex;
    int distance;

    public Node(int vertex, int distance) {
      this.vertex = vertex;
      this.distance = distance;
    }

    @Override
    public int compareTo(Node other) {
      return Integer.compare(this.distance, other.distance);
    }
  }

  /**
   * Constructor to create a graph with given number of vertices.
   * 
   * @param vertices Number of vertices in the graph
   */
  @SuppressWarnings("unchecked")
  public DijkstraAlgorithm(int vertices) {
    this.vertices = vertices;
    adjacencyList = new ArrayList[vertices];

    for (int i = 0; i < vertices; i++) {
      adjacencyList[i] = new ArrayList<>();
    }
  }

  /**
   * Adds a directed edge with weight to the graph.
   * 
   * @param source      Source vertex
   * @param destination Destination vertex
   * @param weight      Weight of the edge
   */
  public void addEdge(int source, int destination, int weight) {
    adjacencyList[source].add(new Edge(destination, weight));
  }

  /**
   * Adds an undirected edge with weight to the graph.
   * 
   * @param source      Source vertex
   * @param destination Destination vertex
   * @param weight      Weight of the edge
   */
  public void addUndirectedEdge(int source, int destination, int weight) {
    adjacencyList[source].add(new Edge(destination, weight));
    adjacencyList[destination].add(new Edge(source, weight));
  }

  /**
   * Finds shortest paths from source to all vertices using Dijkstra's algorithm.
   * 
   * @param source Source vertex
   * @return Array of shortest distances from source to each vertex
   */
  public int[] dijkstra(int source) {
    int[] distance = new int[vertices];
    boolean[] visited = new boolean[vertices];
    PriorityQueue<Node> pq = new PriorityQueue<>();

    Arrays.fill(distance, Integer.MAX_VALUE);
    distance[source] = 0;

    pq.offer(new Node(source, 0));

    while (!pq.isEmpty()) {
      Node current = pq.poll();
      int u = current.vertex;

      if (visited[u]) {
        continue;
      }

      visited[u] = true;

      for (Edge edge : adjacencyList[u]) {
        int v = edge.destination;
        int weight = edge.weight;

        if (!visited[v] && distance[u] != Integer.MAX_VALUE
            && distance[u] + weight < distance[v]) {
          distance[v] = distance[u] + weight;
          pq.offer(new Node(v, distance[v]));
        }
      }
    }

    return distance;
  }

  /**
   * Finds shortest path from source to destination with path reconstruction.
   * 
   * @param source      Source vertex
   * @param destination Destination vertex
   * @return List representing the shortest path
   */
  public List<Integer> findShortestPath(int source, int destination) {
    int[] distance = new int[vertices];
    int[] previous = new int[vertices];
    boolean[] visited = new boolean[vertices];
    PriorityQueue<Node> pq = new PriorityQueue<>();

    Arrays.fill(distance, Integer.MAX_VALUE);
    Arrays.fill(previous, -1);
    distance[source] = 0;

    pq.offer(new Node(source, 0));

    while (!pq.isEmpty()) {
      Node current = pq.poll();
      int u = current.vertex;

      if (visited[u]) {
        continue;
      }

      visited[u] = true;

      if (u == destination) {
        break;
      }

      for (Edge edge : adjacencyList[u]) {
        int v = edge.destination;
        int weight = edge.weight;

        if (!visited[v] && distance[u] != Integer.MAX_VALUE
            && distance[u] + weight < distance[v]) {
          distance[v] = distance[u] + weight;
          previous[v] = u;
          pq.offer(new Node(v, distance[v]));
        }
      }
    }

    List<Integer> path = new ArrayList<>();
    if (previous[destination] == -1 && destination != source) {
      return path;
    }

    for (int at = destination; at != -1; at = previous[at]) {
      path.add(at);
    }

    Collections.reverse(path);
    return path;
  }

  /**
   * Gets the distance of the shortest path between two vertices.
   * 
   * @param source      Source vertex
   * @param destination Destination vertex
   * @return Distance of shortest path, or Integer.MAX_VALUE if no path exists
   */
  public int getShortestDistance(int source, int destination) {
    int[] distances = dijkstra(source);
    return distances[destination];
  }

  /**
   * Utility method to print the graph structure.
   */
  public void printGraph() {
    System.out.println("\nGraph Structure:");
    for (int i = 0; i < vertices; i++) {
      System.out.print("Vertex " + i + " -> ");
      for (Edge edge : adjacencyList[i]) {
        System.out.print(edge.destination + "(weight:" + edge.weight + ") ");
      }
      System.out.println();
    }
  }

  /**
   * Prints shortest distances from source to all vertices.
   */
  public void printShortestDistances(int source) {
    int[] distances = dijkstra(source);
    System.out.println("\nShortest distances from vertex " + source + ":");
    for (int i = 0; i < vertices; i++) {
      if (distances[i] == Integer.MAX_VALUE) {
        System.out.println("To vertex " + i + ": UNREACHABLE");
      } else {
        System.out.println("To vertex " + i + ": " + distances[i]);
      }
    }
  }

  /**
   * Main method with example usage
   */
  public static void main(String[] args) {
    System.out.println("Dijkstra's Algorithm Examples:");

    // Example 1: Simple directed graph
    System.out.println("\n=== Example 1: Basic Shortest Path (Directed Graph) ===");
    DijkstraAlgorithm graph1 = new DijkstraAlgorithm(5);
    graph1.addEdge(0, 1, 4);
    graph1.addEdge(0, 2, 1);
    graph1.addEdge(2, 1, 2);
    graph1.addEdge(1, 3, 1);
    graph1.addEdge(2, 3, 5);
    graph1.addEdge(3, 4, 3);

    graph1.printGraph();
    graph1.printShortestDistances(0);

    // Example 2: Path reconstruction
    System.out.println("\n=== Example 2: Path Reconstruction ===");
    List<Integer> path = graph1.findShortestPath(0, 4);
    int distance = graph1.getShortestDistance(0, 4);
    System.out.println("Shortest path from 0 to 4: " + path);
    System.out.println("Distance: " + distance);

    // Example 3: Undirected graph
    System.out.println("\n=== Example 3: Undirected Graph ===");
    DijkstraAlgorithm graph2 = new DijkstraAlgorithm(6);
    graph2.addUndirectedEdge(0, 1, 2);
    graph2.addUndirectedEdge(0, 2, 4);
    graph2.addUndirectedEdge(1, 2, 1);
    graph2.addUndirectedEdge(1, 3, 7);
    graph2.addUndirectedEdge(2, 4, 3);
    graph2.addUndirectedEdge(3, 4, 2);
    graph2.addUndirectedEdge(3, 5, 1);
    graph2.addUndirectedEdge(4, 5, 5);

    graph2.printGraph();

    List<Integer> path2 = graph2.findShortestPath(0, 5);
    int distance2 = graph2.getShortestDistance(0, 5);
    System.out.println("\nShortest path from 0 to 5: " + path2);
    System.out.println("Distance: " + distance2);

    // Example 4: Graph with unreachable vertices
    System.out.println("\n=== Example 4: Graph with Unreachable Vertices ===");
    DijkstraAlgorithm graph3 = new DijkstraAlgorithm(4);
    graph3.addEdge(0, 1, 1);
    graph3.addEdge(1, 2, 2);

    graph3.printGraph();
    graph3.printShortestDistances(0);

    // Example 5: Multiple paths - finding the shortest
    System.out.println("\n=== Example 5: Multiple Paths ===");
    DijkstraAlgorithm graph4 = new DijkstraAlgorithm(4);
    graph4.addEdge(0, 1, 1);
    graph4.addEdge(0, 2, 4);
    graph4.addEdge(1, 2, 2);
    graph4.addEdge(1, 3, 6);
    graph4.addEdge(2, 3, 3);

    graph4.printGraph();

    System.out.println("\nPath from 0 to 3 (direct): " + graph4.findShortestPath(0, 3));
    System.out.println("Path from 0 to 3 via vertices: " + graph4.findShortestPath(0, 3));
    System.out.println("Distance: " + graph4.getShortestDistance(0, 3));
  }
}
