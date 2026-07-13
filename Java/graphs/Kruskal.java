package graphs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

// Kruskal MST. O(E log E).
public class Kruskal {

    static class Edge {
        final int u, v, w;

        Edge(int u, int v, int w) {
            this.u = u;
            this.v = v;
            this.w = w;
        }
    }

    static class UnionFind {
        private final int[] parent;
        private final int[] rank;

        UnionFind(int n) {
            parent = new int[n];
            rank = new int[n];
            for (int i = 0; i < n; i++) {
                parent[i] = i;
            }
        }

        int find(int x) {
            if (parent[x] != x) {
                parent[x] = find(parent[x]);
            }
            return parent[x];
        }

        boolean union(int a, int b) {
            int ra = find(a);
            int rb = find(b);
            if (ra == rb) {
                return false;
            }
            if (rank[ra] < rank[rb]) {
                parent[ra] = rb;
            } else if (rank[ra] > rank[rb]) {
                parent[rb] = ra;
            } else {
                parent[rb] = ra;
                rank[ra]++;
            }
            return true;
        }
    }

    static class Result {
        final List<Edge> edges;
        final long totalWeight;

        Result(List<Edge> edges, long totalWeight) {
            this.edges = edges;
            this.totalWeight = totalWeight;
        }
    }

    public static Result mst(int n, Edge[] edges) {
        Edge[] sorted = Arrays.copyOf(edges, edges.length);
        Arrays.sort(sorted, Comparator.comparingInt(e -> e.w));

        UnionFind uf = new UnionFind(n);
        List<Edge> mstEdges = new ArrayList<>();
        long totalWeight = 0;

        for (Edge e : sorted) {
            if (uf.union(e.u, e.v)) {
                mstEdges.add(e);
                totalWeight += e.w;
                if (mstEdges.size() == n - 1) {
                    break;
                }
            }
        }

        return new Result(mstEdges, totalWeight);
    }

    public static void main(String[] args) {
        Edge[] edges = {
            new Edge(0, 1, 10),
            new Edge(0, 2, 6),
            new Edge(0, 3, 5),
            new Edge(1, 3, 15),
            new Edge(2, 3, 4)
        };
        Result r = mst(4, edges);
        System.out.println("MST weight: " + r.totalWeight);
        for (Edge e : r.edges) {
            System.out.println(e.u + " - " + e.v + " : " + e.w);
        }
    }
}
