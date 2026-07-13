package graphs;

// Floyd-Warshall all-pairs shortest paths. O(V³) time O(V²) space.
public class FloydWarshall {
    public static final int INF = 1_000_000_000;

    public static void floyd(int[][] dist) {
        int n = dist.length;
        for (int k = 0; k < n; k++) {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    if (dist[i][k] < INF && dist[k][j] < INF
                            && dist[i][k] + dist[k][j] < dist[i][j]) {
                        dist[i][j] = dist[i][k] + dist[k][j];
                    }
                }
            }
        }
    }

    public static void main(String[] args) {
        int[][] dist = {
            {0, 5, INF, 10},
            {INF, 0, 3, INF},
            {INF, INF, 0, 1},
            {INF, INF, INF, 0}
        };
        floyd(dist);
        for (int i = 0; i < dist.length; i++) {
            for (int j = 0; j < dist[i].length; j++) {
                System.out.print((dist[i][j] >= INF ? "INF" : dist[i][j]) + " ");
            }
            System.out.println();
        }
    }
}
