
/**
 * Breadth-First Search (BFS) Algorithm
 * 
 * Time Complexity: O(V + E) where V is vertices and E is edges
 * Space Complexity: O(V)
 * 
 * Graph traversal algorithm that explores all vertices at the present depth
 * before moving to vertices at the next depth level.
 */
class BreadthFirstSearch {
  /**
   * Performs BFS traversal starting from a given vertex.
   * 
   * @param graph Adjacency list representation of the graph
   * @param start Starting vertex
   * @return Array of vertices in BFS order
   */
  static traverse(graph: number[][], start: number): number[] {
    const n = graph.length;
    const visited = new Array(n).fill(false);
    const result: number[] = [];
    const queue: number[] = [];

    visited[start] = true;
    queue.push(start);

    while (queue.length > 0) {
      const vertex = queue.shift()!;
      result.push(vertex);

      for (const neighbor of graph[vertex]) {
        if (!visited[neighbor]) {
          visited[neighbor] = true;
          queue.push(neighbor);
        }
      }
    }

    return result;
  }

  /**
   * Finds the shortest path between two vertices in an unweighted graph.
   * 
   * @param graph Adjacency list representation of the graph
   * @param start Starting vertex
   * @param end Target vertex
   * @return Array representing the shortest path, or empty array if no path exists
   */
  static shortestPath(graph: number[][], start: number, end: number): number[] {
    const n = graph.length;
    const visited = new Array(n).fill(false);
    const parent = new Array(n).fill(-1);
    const queue: number[] = [];

    visited[start] = true;
    queue.push(start);

    while (queue.length > 0) {
      const vertex = queue.shift()!;

      if (vertex === end) {
        return this.reconstructPath(parent, start, end);
      }

      for (const neighbor of graph[vertex]) {
        if (!visited[neighbor]) {
          visited[neighbor] = true;
          parent[neighbor] = vertex;
          queue.push(neighbor);
        }
      }
    }

    return []; // No path found
  }

  /**
   * Checks if the graph is connected (all vertices are reachable from start).
   */
  static isConnected(graph: number[][], start: number = 0): boolean {
    const n = graph.length;
    const visited = this.traverse(graph, start);
    return visited.length === n;
  }

  /**
   * Reconstructs the path from parent array.
   */
  private static reconstructPath(parent: number[], start: number, end: number): number[] {
    const path: number[] = [];
    let current = end;

    while (current !== -1) {
      path.unshift(current);
      if (current === start) break;
      current = parent[current];
    }

    return path;
  }
}
