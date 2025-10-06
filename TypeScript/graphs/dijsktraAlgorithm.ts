/**
 * Dijkstra's Algorithm
 * 
 * Time Complexity: O((V + E) log V) with priority queue
 * Space Complexity: O(V)
 * 
 * Shortest path algorithm for graphs with non-negative edge weights.
 * Finds the shortest path from a source vertex to all other vertices.
 */
class DijkstraAlgorithm {
  /**
   * Finds shortest distances from source to all vertices.
   * 
   * @param graph Adjacency list where each element is [neighbor, weight]
   * @param source Source vertex
   * @return Array of shortest distances from source to each vertex
   */
  static shortestDistances(graph: [number, number][][], source: number): number[] {
    const n = graph.length;
    const distances = new Array(n).fill(Infinity);
    const visited = new Array(n).fill(false);

    distances[source] = 0;

    for (let i = 0; i < n; i++) {
      const u = this.getMinDistanceVertex(distances, visited);

      if (u === -1) break;

      visited[u] = true;

      for (const [neighbor, weight] of graph[u]) {
        if (!visited[neighbor]) {
          const newDist = distances[u] + weight;
          if (newDist < distances[neighbor]) {
            distances[neighbor] = newDist;
          }
        }
      }
    }

    return distances;
  }

  /**
   * Finds the shortest path from source to target.
   * 
   * @param graph Adjacency list where each element is [neighbor, weight]
   * @param source Source vertex
   * @param target Target vertex
   * @return Object containing path array and total distance
   */
  static shortestPath(
    graph: [number, number][][],
    source: number,
    target: number
  ): { path: number[]; distance: number } {
    const n = graph.length;
    const distances = new Array(n).fill(Infinity);
    const visited = new Array(n).fill(false);
    const parent = new Array(n).fill(-1);

    distances[source] = 0;

    for (let i = 0; i < n; i++) {
      const u = this.getMinDistanceVertex(distances, visited);

      if (u === -1 || u === target) break;

      visited[u] = true;

      for (const [neighbor, weight] of graph[u]) {
        if (!visited[neighbor]) {
          const newDist = distances[u] + weight;
          if (newDist < distances[neighbor]) {
            distances[neighbor] = newDist;
            parent[neighbor] = u;
          }
        }
      }
    }

    if (distances[target] === Infinity) {
      return { path: [], distance: Infinity };
    }

    const path = this.reconstructPath(parent, source, target);
    return { path, distance: distances[target] };
  }

  /**
   * Gets the vertex with minimum distance that hasn't been visited.
   */
  private static getMinDistanceVertex(distances: number[], visited: boolean[]): number {
    let minDist = Infinity;
    let minIndex = -1;

    for (let i = 0; i < distances.length; i++) {
      if (!visited[i] && distances[i] < minDist) {
        minDist = distances[i];
        minIndex = i;
      }
    }

    return minIndex;
  }

  /**
   * Reconstructs the path from parent array.
   */
  private static reconstructPath(parent: number[], source: number, target: number): number[] {
    const path: number[] = [];
    let current = target;

    while (current !== -1) {
      path.unshift(current);
      if (current === source) break;
      current = parent[current];
    }

    return path;
  }

  /**
   * Finds shortest paths from source to all vertices using priority queue optimization.
   * This is more efficient for sparse graphs.
   * 
   * @param graph Adjacency list where each element is [neighbor, weight]
   * @param source Source vertex
   * @return Array of shortest distances from source to each vertex
   */
  static shortestDistancesOptimized(graph: [number, number][][], source: number): number[] {
    const n = graph.length;
    const distances = new Array(n).fill(Infinity);
    const pq: [number, number][] = []; // [distance, vertex]

    distances[source] = 0;
    pq.push([0, source]);

    while (pq.length > 0) {
      // Get vertex with minimum distance (simple extraction, not true priority queue)
      pq.sort((a, b) => a[0] - b[0]);
      const [currentDist, u] = pq.shift()!;

      if (currentDist > distances[u]) continue;

      for (const [neighbor, weight] of graph[u]) {
        const newDist = distances[u] + weight;
        if (newDist < distances[neighbor]) {
          distances[neighbor] = newDist;
          pq.push([newDist, neighbor]);
        }
      }
    }

    return distances;
  }
}
