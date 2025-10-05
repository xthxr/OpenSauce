import java.util.*;

/**
 * Breadth-First Search (BFS) Algorithm
 * 
 * Time Complexity: O(V + E) where V is vertices and E is edges
 * Space Complexity: O(V) for the queue and visited array
 * 
 * BFS explores all vertices at the present depth before moving to vertices
 * at the next depth level. It uses a queue data structure.
 */
public class BreadthFirstSearch {

  private int vertices;
  private LinkedList<Integer>[] adjacencyList;

  /**
   * Constructor to create a graph with given number of vertices.
   * 
   * @param vertices Number of vertices in the graph
   */
  @SuppressWarnings("unchecked")
  public BreadthFirstSearch(int vertices) {
    this.vertices = vertices;
    adjacencyList = new LinkedList[vertices];

    for (int i = 0; i < vertices; i++) {
      adjacencyList[i] = new LinkedList<>();
    }
  }

  /**
   * Adds an undirected edge to the graph.
   * 
   * @param source      Source vertex
   * @param destination Destination vertex
   */
  public void addEdge(int source, int destination) {
    adjacencyList[source].add(destination);
    adjacencyList[destination].add(source);
  }

  /**
   * Adds a directed edge to the graph.
   * 
   * @param source      Source vertex
   * @param destination Destination vertex
   */
  public void addDirectedEdge(int source, int destination) {
    adjacencyList[source].add(destination);
  }

  /**
   * Performs BFS traversal starting from a given vertex.
   * 
   * @param startVertex Starting vertex for BFS
   * @return List of vertices in BFS order
   */
  public List<Integer> bfs(int startVertex) {
    List<Integer> result = new ArrayList<>();
    boolean[] visited = new boolean[vertices];
    Queue<Integer> queue = new LinkedList<>();

    visited[startVertex] = true;
    queue.offer(startVertex);

    while (!queue.isEmpty()) {
      int currentVertex = queue.poll();
      result.add(currentVertex);

      for (int neighbor : adjacencyList[currentVertex]) {
        if (!visited[neighbor]) {
          visited[neighbor] = true;
          queue.offer(neighbor);
        }
      }
    }

    return result;
  }

  /**
   * Finds the shortest path from source to destination using BFS.
   * 
   * @param source      Source vertex
   * @param destination Destination vertex
   * @return List representing the shortest path, or empty list if no path exists
   */
  public List<Integer> findShortestPath(int source, int destination) {
    boolean[] visited = new boolean[vertices];
    int[] parent = new int[vertices];
    Queue<Integer> queue = new LinkedList<>();

    Arrays.fill(parent, -1);
    visited[source] = true;
    queue.offer(source);

    while (!queue.isEmpty()) {
      int current = queue.poll();

      if (current == destination) {
        break;
      }

      for (int neighbor : adjacencyList[current]) {
        if (!visited[neighbor]) {
          visited[neighbor] = true;
          parent[neighbor] = current;
          queue.offer(neighbor);
        }
      }
    }

    if (!visited[destination]) {
      return new ArrayList<>();
    }

    List<Integer> path = new ArrayList<>();
    for (int at = destination; at != -1; at = parent[at]) {
      path.add(at);
    }
    Collections.reverse(path);

    return path;
  }

  /**
   * Finds the distance (number of edges) from source to destination.
   * 
   * @param source      Source vertex
   * @param destination Destination vertex
   * @return Distance in number of edges, or -1 if no path exists
   */
  public int findDistance(int source, int destination) {
    boolean[] visited = new boolean[vertices];
    int[] distance = new int[vertices];
    Queue<Integer> queue = new LinkedList<>();

    Arrays.fill(distance, -1);
    visited[source] = true;
    distance[source] = 0;
    queue.offer(source);

    while (!queue.isEmpty()) {
      int current = queue.poll();

      if (current == destination) {
        return distance[destination];
      }

      for (int neighbor : adjacencyList[current]) {
        if (!visited[neighbor]) {
          visited[neighbor] = true;
          distance[neighbor] = distance[current] + 1;
          queue.offer(neighbor);
        }
      }
    }

    return -1;
  }

  /**
   * Finds all vertices at a given distance from the source.
   * 
   * @param source   Source vertex
   * @param distance Target distance
   * @return List of vertices at the given distance
   */
  public List<Integer> findVerticesAtDistance(int source, int distance) {
    List<Integer> result = new ArrayList<>();
    boolean[] visited = new boolean[vertices];
    int[] dist = new int[vertices];
    Queue<Integer> queue = new LinkedList<>();

    visited[source] = true;
    dist[source] = 0;
    queue.offer(source);

    while (!queue.isEmpty()) {
      int current = queue.poll();

      if (dist[current] == distance) {
        result.add(current);
      }

      if (dist[current] < distance) {
        for (int neighbor : adjacencyList[current]) {
          if (!visited[neighbor]) {
            visited[neighbor] = true;
            dist[neighbor] = dist[current] + 1;
            queue.offer(neighbor);
          }
        }
      }
    }

    return result;
  }

  /**
   * Checks if the graph is bipartite using BFS.
   * 
   * @return true if graph is bipartite, false otherwise
   */
  public boolean isBipartite() {
    int[] color = new int[vertices];
    Arrays.fill(color, -1);

    for (int i = 0; i < vertices; i++) {
      if (color[i] == -1) {
        if (!isBipartiteHelper(i, color)) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * Helper method for bipartite checking.
   */
  private boolean isBipartiteHelper(int start, int[] color) {
    Queue<Integer> queue = new LinkedList<>();
    color[start] = 0;
    queue.offer(start);

    while (!queue.isEmpty()) {
      int current = queue.poll();

      for (int neighbor : adjacencyList[current]) {
        if (color[neighbor] == -1) {
          color[neighbor] = 1 - color[current];
          queue.offer(neighbor);
        } else if (color[neighbor] == color[current]) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * Utility method to print the graph structure.
   */
  public void printGraph() {
    System.out.println("\nGraph Structure:");
    for (int i = 0; i < vertices; i++) {
      System.out.print("Vertex " + i + " -> ");
      for (int neighbor : adjacencyList[i]) {
        System.out.print(neighbor + " ");
      }
      System.out.println();
    }
  }

  /**
   * Main method with example usage
   */
  public static void main(String[] args) {
    System.out.println("Breadth-First Search (BFS) Examples:");

    // Example 1: BFS Traversal
    System.out.println("\n=== Example 1: BFS Traversal ===");
    BreadthFirstSearch graph1 = new BreadthFirstSearch(6);
    graph1.addEdge(0, 1);
    graph1.addEdge(0, 2);
    graph1.addEdge(1, 3);
    graph1.addEdge(1, 4);
    graph1.addEdge(2, 4);
    graph1.addEdge(3, 5);
    graph1.addEdge(4, 5);

    graph1.printGraph();
    System.out.println("\nBFS from vertex 0: " + graph1.bfs(0));
    System.out.println("BFS from vertex 2: " + graph1.bfs(2));

    // Example 2: Shortest path finding
    System.out.println("\n=== Example 2: Shortest Path ===");
    List<Integer> path = graph1.findShortestPath(0, 5);
    System.out.println("Shortest path from 0 to 5: " + path);
    System.out.println("Distance: " + graph1.findDistance(0, 5));

    path = graph1.findShortestPath(0, 3);
    System.out.println("Shortest path from 0 to 3: " + path);
    System.out.println("Distance: " + graph1.findDistance(0, 3));

    // Example 3: Vertices at specific distance
    System.out.println("\n=== Example 3: Vertices at Distance ===");
    System.out.println("Vertices at distance 1 from vertex 0: " + graph1.findVerticesAtDistance(0, 1));
    System.out.println("Vertices at distance 2 from vertex 0: " + graph1.findVerticesAtDistance(0, 2));

    // Example 4: Bipartite check
    System.out.println("\n=== Example 4: Bipartite Graph Check ===");
    BreadthFirstSearch graph2 = new BreadthFirstSearch(4);
    graph2.addEdge(0, 1);
    graph2.addEdge(1, 2);
    graph2.addEdge(2, 3);
    graph2.addEdge(3, 0);

    graph2.printGraph();
    System.out.println("Is bipartite? " + graph2.isBipartite());

    BreadthFirstSearch graph3 = new BreadthFirstSearch(3);
    graph3.addEdge(0, 1);
    graph3.addEdge(1, 2);
    graph3.addEdge(2, 0);

    graph3.printGraph();
    System.out.println("Is bipartite? " + graph3.isBipartite());

    // Example 5: Disconnected graph
    System.out.println("\n=== Example 5: Disconnected Graph ===");
    BreadthFirstSearch graph4 = new BreadthFirstSearch(7);
    graph4.addEdge(0, 1);
    graph4.addEdge(0, 2);
    graph4.addEdge(3, 4);
    graph4.addEdge(5, 6);

    graph4.printGraph();
    System.out.println("BFS from vertex 0: " + graph4.bfs(0));
    System.out.println("BFS from vertex 3: " + graph4.bfs(3));
    System.out.println("Path from 0 to 5: " + graph4.findShortestPath(0, 5));
  }
}
