import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * LeetCode Problem.
 * It can be seen as graph problem.
 * Solved using modified BFS, Bellman-Ford Algorithm
 * (https://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm).
 * 
 * <p>
 * 
 * Problem:
 * There are n cities connected by some number of flights. You are given an
 * array flights where flights[i] = [from_i, to_i, price_i] indicates that there
 * is a flight from city from_i to city to_i with cost price_i. You are also
 * given three integers src, dst, and k, return the cheapest price from src to
 * dst with at most k stops. If there is no such route, return -1.
 */
public class FindCheapestPriceWithKStop {

  public static int findCheapestPrice(int n, int[][] flights, int src, int dst, int k) {
    Map<Integer, Map<Integer, Integer>> adjList = new HashMap<>();
    for (int[] f : flights) {
      // build the adjency list first
      adjList.computeIfAbsent(f[0], HashMap::new)
          .put(f[1], f[2]);
    }
    ArrayDeque<int[]> q = new ArrayDeque<>(); // data structure that works for both queue & stack functionalities
    q.offer(new int[] { src, 0, 0 }); // vertex/node, cost, stops
    int[] dist = new int[n];
    // fill initial route as no-route with using int max value
    Arrays.fill(dist, Integer.MAX_VALUE);
    dist[src] = 0;
    while (!q.isEmpty()) {
      // do the search
      var cur = q.poll();
      int node = cur[0];
      int cost = cur[1];
      int stops = cur[2];
      if (stops > k) {
        // stop condition.
        // it prunes the path that has exceeded the maximum number of allowed stops
        continue;
      }
      for (var ne : adjList.getOrDefault(node, new HashMap<>()).entrySet()) {
        int tn = ne.getKey();
        int tc = ne.getValue() + cost;
        if (tc < dist[tn]) {
          q.offer(new int[] { tn, tc, stops + 1 });
          dist[tn] = tc;
        }
      }
    }
    // O(k . E)
    return (dist[dst] == Integer.MAX_VALUE) ? -1 : dist[dst];
  }

  public static void main(String[] args) {
    int result = FindCheapestPriceWithKStop.findCheapestPrice(
        3,
        new int[][] { { 0, 1, 100 }, { 1, 2, 100 }, { 0, 2, 500 } },
        0, 2, 0);
    // The optimal path with no stops from city 0 to 2 and has cost 500.
    assertThis(result == 500);
  }

  private static void assertThis(boolean condition) {
    if (!condition) {
      throw new AssertionError();
    }
  }
}
