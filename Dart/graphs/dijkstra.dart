/*
    Problem Statement :-
    -----------------
    Given a weighted, connected graph, find the shortest distance from the source node
    to all other nodes using Dijkstra's Algorithm.

    Algorithm :-
    -----------
    1. Create an adjacency list where each node maps to a list of edges (to, weight).
    2. Initialize a distance map 'dist' with infinity (double.infinity).
    3. Use a min-priority queue to always pick the node with the smallest known distance.
    4. Set dist[source] = 0 and push (source, 0) into the priority queue.
    5. While the queue is not empty:
        a. Extract the node with the smallest distance.
        b. For each neighbor (v, w), if dist[u] + w < dist[v],
           update dist[v] and push (v, dist[v]) into the queue.
    6. Return the distance map.

    Example :-
    ----------
    Graph :-
    A -> B(4), C(2)
    B -> C(5), D(10)
    C -> D(3)
    D -> (no outgoing edges)
    E -> A(1)

    Output :-
    Shortest distances from node A:
      A : 0.0
      B : 4.0
      C : 2.0
      D : 5.0
      E : ∞
*/


import 'package:collection/collection.dart';

/// Represents an edge from `to` with weight `weight`.
class Edge<T> {
  final T to;
  final double weight;

  Edge(this.to, this.weight);

  @override
  String toString() => 'Edge(to: $to, weight: $weight)';
}

/// Computes shortest path distances from [source] to all nodes in a graph
/// represented by an adjacency list.
///
/// [graph] maps each node to the list of outgoing edges.
/// If the graph is disconnected, unreachable nodes will have distance `double.infinity`.
///
/// Returns a map of node → minimum distance from [source].
///
/// Complexity: O((V + E) log V) using a min-heap priority queue.
Map<T, double> dijkstra<T>(Map<T, List<Edge<T>>> graph, T source) {
  // Initialize distances to infinity
  final Map<T, double> dist = {for (var node in graph.keys) node: double.infinity};

  // Include nodes that appear only as edge targets
  for (var entry in graph.entries) {
    for (var e in entry.value) {
      dist.putIfAbsent(e.to, () => double.infinity);
    }
  }

  // Distance to source is 0
  dist[source] = 0.0;

  // Priority queue to process the node with the smallest current distance
  final pq = PriorityQueue<_NodeDist<T>>((a, b) => a.dist.compareTo(b.dist));
  pq.add(_NodeDist(source, 0.0));

  while (pq.isNotEmpty) {
    final current = pq.removeFirst();
    final node = current.node;
    final nodeDist = current.dist;

    // Skip if we already found a better path
    if (nodeDist > (dist[node] ?? double.infinity)) continue;

    final neighbours = graph[node] ?? const [];
    for (final edge in neighbours) {
      final next = edge.to;
      final weight = edge.weight;
      final newDist = nodeDist + weight;

      if (newDist < (dist[next] ?? double.infinity)) {
        dist[next] = newDist;
        pq.add(_NodeDist(next, newDist));
      }
    }
  }

  return dist;
}

/// Helper class used in the priority queue to hold nodes with current best distance.
class _NodeDist<T> {
  final T node;
  final double dist;

  _NodeDist(this.node, this.dist);

  @override
  String toString() => '(_node: $node, dist: $dist)';
}

/// Example usage
void main() {
  // Example graph: V = 5 (A, B, C, D, E) with weighted edges
  final graph = <String, List<Edge<String>>>{
    'A': [Edge('B', 4), Edge('C', 2)],
    'B': [Edge('C', 5), Edge('D', 10)],
    'C': [Edge('D', 3)],
    'D': [],
    'E': [Edge('A', 1)] // disconnected part
  };

  final source = 'A';
  final distances = dijkstra(graph, source);

  print('Shortest distances from node $source:');
  for (var entry in distances.entries) {
    final node = entry.key;
    final d = entry.value;
    final dStr = (d == double.infinity) ? '∞' : d.toString();
    print('  $node : $dStr');
  }

  // Expected output for reachable nodes:
  //   A : 0.0
  //   B : 4.0
  //   C : 2.0
  //   D : 5.0
  //   E : ∞
}
