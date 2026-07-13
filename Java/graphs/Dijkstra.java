/*
 * Dijkstra single-source shortest paths on non-negative weighted digraph.
 * Adjacency list + binary heap. O((V+E) log V) time, O(V) space.
 */

import java.util.*;

public class Dijkstra {

    // g: List of adjacency lists; each edge is int[]{to, weight}
    public static int[] dijkstra(int n, List<List<int[]>> g, int src) {
        int[] dist = new int[n];
        Arrays.fill(dist, Integer.MAX_VALUE);
        dist[src] = 0;

        PriorityQueue<int[]> pq = new PriorityQueue<>(Comparator.comparingInt(a -> a[0]));
        pq.add(new int[]{0, src}); // {dist, node}

        while (!pq.isEmpty()) {
            int[] cur = pq.poll();
            int d = cur[0], u = cur[1];
            if (d > dist[u]) continue; // skip stale

            for (int[] e : g.get(u)) {
                int v = e[0], w = e[1];
                if (dist[u] + w < dist[v]) {
                    dist[v] = dist[u] + w;
                    pq.add(new int[]{dist[v], v});
                }
            }
        }
        return dist;
    }

    public static void main(String[] args) {
        int n = 5;
        List<List<int[]>> g = new ArrayList<>();
        for (int i = 0; i < n; i++) g.add(new ArrayList<>());

        g.get(0).add(new int[]{1, 2});
        g.get(0).add(new int[]{2, 4});
        g.get(1).add(new int[]{2, 1});
        g.get(1).add(new int[]{3, 7});
        g.get(2).add(new int[]{4, 3});
        g.get(3).add(new int[]{4, 1});

        int[] dist = dijkstra(n, g, 0);
        System.out.println(Arrays.toString(dist));
    }
}
