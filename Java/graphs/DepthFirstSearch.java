import java.util.*;

/**
 * Depth-First Search (DFS) Algorithm
 * 
 * Time Complexity: O(V + E) where V is vertices and E is edges
 * Space Complexity: O(V) for the recursion stack and visited array
 * 
 * DFS explores as far as possible along each branch before backtracking.
 */
public class DepthFirstSearch {

  private int vertices;
  private LinkedList<Integer>[] adjacencyList;

  /**
   * Constructor to create a graph with given number of vertices.
   * 
   * @param vertices Number of vertices in the graph
   */
  @SuppressWarnings("unchecked")
  public DepthFirstSearch(int vertices) {
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
   * Performs DFS traversal starting from a given vertex (recursive).
   * 
   * @param startVertex Starting vertex for DFS
   * @return List of vertices in DFS order
   */
  public List<Integer> dfsRecursive(int startVertex) {
    List<Integer> result = new ArrayList<>();
    boolean[] visited = new boolean[vertices];
    dfsRecursiveHelper(startVertex, visited, result);
    return result;
  }

  /**
   * Helper method for recursive DFS.
   */
  private void dfsRecursiveHelper(int vertex, boolean[] visited, List<Integer> result) {
    visited[vertex] = true;
    result.add(vertex);

    for (int neighbor : adjacencyList[vertex]) {
      if (!visited[neighbor]) {
        dfsRecursiveHelper(neighbor, visited, result);
      }
    }
  }

  /**
   * Performs DFS traversal starting from a given vertex (iterative using stack).
   * 
   * @param startVertex Starting vertex for DFS
   * @return List of vertices in DFS order
   */
  public List<Integer> dfsIterative(int startVertex) {
    List<Integer> result = new ArrayList<>();
    boolean[] visited = new boolean[vertices];
    Stack<Integer> stack = new Stack<>();

    stack.push(startVertex);

    while (!stack.isEmpty()) {
      int currentVertex = stack.pop();

      if (!visited[currentVertex]) {
        visited[currentVertex] = true;
        result.add(currentVertex);

        List<Integer> neighbors = new ArrayList<>(adjacencyList[currentVertex]);
        Collections.reverse(neighbors);

        for (int neighbor : neighbors) {
          if (!visited[neighbor]) {
            stack.push(neighbor);
          }
        }
      }
    }

    return result;
  }

  /**
   * Finds a path from source to destination using DFS.
   * 
   * @param source      Source vertex
   * @param destination Destination vertex
   * @return List representing a path, or empty list if no path exists
   */
  public List<Integer> findPath(int source, int destination) {
    List<Integer> path = new ArrayList<>();
    boolean[] visited = new boolean[vertices];

    if (findPathHelper(source, destination, visited, path)) {
      return path;
    }

    return new ArrayList<>();
  }

  /**
   * Helper method for finding a path using DFS.
   */
  private boolean findPathHelper(int current, int destination, boolean[] visited, List<Integer> path) {
    visited[current] = true;
    path.add(current);

    if (current == destination) {
      return true;
    }

    for (int neighbor : adjacencyList[current]) {
      if (!visited[neighbor]) {
        if (findPathHelper(neighbor, destination, visited, path)) {
          return true;
        }
      }
    }

    path.remove(path.size() - 1);
    return false;
  }

  /**
   * Detects if the graph contains a cycle (undirected graph).
   * 
   * @return true if cycle exists, false otherwise
   */
  public boolean hasCycle() {
    boolean[] visited = new boolean[vertices];

    for (int i = 0; i < vertices; i++) {
      if (!visited[i]) {
        if (hasCycleHelper(i, visited, -1)) {
          return true;
        }
      }
    }

    return false;
  }

  /**
   * Helper method for cycle detection in undirected graph.
   */
  private boolean hasCycleHelper(int vertex, boolean[] visited, int parent) {
    visited[vertex] = true;

    for (int neighbor : adjacencyList[vertex]) {
      if (!visited[neighbor]) {
        if (hasCycleHelper(neighbor, visited, vertex)) {
          return true;
        }
      } else if (neighbor != parent) {
        return true;
      }
    }

    return false;
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
    System.out.println("Depth-First Search (DFS) Examples:");

    // Example 1: DFS Traversal
    System.out.println("\n=== Example 1: DFS Traversal (Recursive vs Iterative) ===");
    DepthFirstSearch graph1 = new DepthFirstSearch(6);
    graph1.addEdge(0, 1);
    graph1.addEdge(0, 2);
    graph1.addEdge(1, 3);
    graph1.addEdge(1, 4);
    graph1.addEdge(2, 4);
    graph1.addEdge(3, 5);
    graph1.addEdge(4, 5);

    graph1.printGraph();

    System.out.println("\nDFS Recursive from vertex 0: " + graph1.dfsRecursive(0));
    System.out.println("DFS Iterative from vertex 0: " + graph1.dfsIterative(0));

    // Example 2: Path finding
    System.out.println("\n=== Example 2: Path Finding ===");
    List<Integer> path = graph1.findPath(0, 5);
    System.out.println("Path from 0 to 5: " + path);

    path = graph1.findPath(0, 3);
    System.out.println("Path from 0 to 3: " + path);

    // Example 3: Cycle detection
    System.out.println("\n=== Example 3: Cycle Detection ===");
    DepthFirstSearch graph2 = new DepthFirstSearch(4);
    graph2.addEdge(0, 1);
    graph2.addEdge(1, 2);
    graph2.addEdge(2, 3);

    graph2.printGraph();
    System.out.println("Has cycle? " + graph2.hasCycle());

    graph2.addEdge(3, 0);
    System.out.println("After adding edge 3-0, has cycle? " + graph2.hasCycle());

    // Example 4: Disconnected graph
    System.out.println("\n=== Example 4: Disconnected Graph ===");
    DepthFirstSearch graph3 = new DepthFirstSearch(7);
    graph3.addEdge(0, 1);
    graph3.addEdge(0, 2);
    graph3.addEdge(3, 4);
    graph3.addEdge(5, 6);

    graph3.printGraph();
    System.out.println("DFS from vertex 0: " + graph3.dfsRecursive(0));
    System.out.println("DFS from vertex 3: " + graph3.dfsRecursive(3));

    // Example 5: Directed graph
    System.out.println("\n=== Example 5: Directed Graph ===");
    DepthFirstSearch graph4 = new DepthFirstSearch(5);
    graph4.addDirectedEdge(0, 1);
    graph4.addDirectedEdge(0, 2);
    graph4.addDirectedEdge(1, 3);
    graph4.addDirectedEdge(2, 3);
    graph4.addDirectedEdge(3, 4);

    graph4.printGraph();
    System.out.println("DFS from vertex 0: " + graph4.dfsRecursive(0));
  }
}
