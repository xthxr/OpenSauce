/**
 * Depth-First Search (DFS) Algorithm
 * 
 * Time Complexity: O(V + E) where V is vertices and E is edges
 * Space Complexity: O(V)
 * 
 * Graph traversal algorithm that explores as far as possible along each branch
 * before backtracking.
 */
class DepthFirstSearch {
  /**
   * Performs DFS traversal starting from a given vertex (iterative approach).
   * 
   * @param graph Adjacency list representation of the graph
   * @param start Starting vertex
   * @return Array of vertices in DFS order
   */
  static traverse(graph: number[][], start: number): number[] {
    const n = graph.length;
    const visited = new Array(n).fill(false);
    const result: number[] = [];
    const stack: number[] = [];

    stack.push(start);

    while (stack.length > 0) {
      const vertex = stack.pop()!;

      if (!visited[vertex]) {
        visited[vertex] = true;
        result.push(vertex);

        // Push neighbors in reverse order to maintain left-to-right traversal
        for (let i = graph[vertex].length - 1; i >= 0; i--) {
          const neighbor = graph[vertex][i];
          if (!visited[neighbor]) {
            stack.push(neighbor);
          }
        }
      }
    }

    return result;
  }

  /**
   * Performs DFS traversal using recursion.
   * 
   * @param graph Adjacency list representation of the graph
   * @param start Starting vertex
   * @return Array of vertices in DFS order
   */
  static traverseRecursive(graph: number[][], start: number): number[] {
    const n = graph.length;
    const visited = new Array(n).fill(false);
    const result: number[] = [];

    this.dfsHelper(graph, start, visited, result);

    return result;
  }

  /**
   * Helper method for recursive DFS.
   */
  private static dfsHelper(
    graph: number[][],
    vertex: number,
    visited: boolean[],
    result: number[]
  ): void {
    visited[vertex] = true;
    result.push(vertex);

    for (const neighbor of graph[vertex]) {
      if (!visited[neighbor]) {
        this.dfsHelper(graph, neighbor, visited, result);
      }
    }
  }

  /**
   * Detects if the graph contains a cycle.
   * 
   * @param graph Adjacency list representation of the graph
   * @return true if cycle exists, false otherwise
   */
  static hasCycle(graph: number[][]): boolean {
    const n = graph.length;
    const visited = new Array(n).fill(false);
    const recStack = new Array(n).fill(false);

    for (let i = 0; i < n; i++) {
      if (!visited[i]) {
        if (this.hasCycleHelper(graph, i, visited, recStack)) {
          return true;
        }
      }
    }

    return false;
  }

  /**
   * Helper method for cycle detection.
   */
  private static hasCycleHelper(
    graph: number[][],
    vertex: number,
    visited: boolean[],
    recStack: boolean[]
  ): boolean {
    visited[vertex] = true;
    recStack[vertex] = true;

    for (const neighbor of graph[vertex]) {
      if (!visited[neighbor]) {
        if (this.hasCycleHelper(graph, neighbor, visited, recStack)) {
          return true;
        }
      } else if (recStack[neighbor]) {
        return true;
      }
    }

    recStack[vertex] = false;
    return false;
  }

  /**
   * Performs topological sort on a directed acyclic graph (DAG).
   * 
   * @param graph Adjacency list representation of the graph
   * @return Array of vertices in topological order, or empty array if cycle exists
   */
  static topologicalSort(graph: number[][]): number[] {
    const n = graph.length;
    const visited = new Array(n).fill(false);
    const stack: number[] = [];

    for (let i = 0; i < n; i++) {
      if (!visited[i]) {
        this.topologicalSortHelper(graph, i, visited, stack);
      }
    }

    return stack.reverse();
  }

  /**
   * Helper method for topological sort.
   */
  private static topologicalSortHelper(
    graph: number[][],
    vertex: number,
    visited: boolean[],
    stack: number[]
  ): void {
    visited[vertex] = true;

    for (const neighbor of graph[vertex]) {
      if (!visited[neighbor]) {
        this.topologicalSortHelper(graph, neighbor, visited, stack);
      }
    }

    stack.push(vertex);
  }
}
