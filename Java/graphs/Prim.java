import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.PriorityQueue;

// O(E log V)
public class Prim {

    public static long prim(int n, List<List<int[]>> g) {
        boolean[] inMST = new boolean[n];
        PriorityQueue<int[]> pq = new PriorityQueue<>(Comparator.comparingInt(a -> a[1]));
        long totalWeight = 0;
        int count = 0;
        pq.offer(new int[] {0, 0});
        while (!pq.isEmpty() && count < n) {
            int[] cur = pq.poll();
            int u = cur[0];
            int w = cur[1];
            if (inMST[u]) {
                continue;
            }
            inMST[u] = true;
            totalWeight += w;
            count++;
            for (int[] e : g.get(u)) {
                if (!inMST[e[0]]) {
                    pq.offer(new int[] {e[0], e[1]});
                }
            }
        }
        return totalWeight;
    }

    public static void main(String[] args) {
        int n = 4;
        List<List<int[]>> g = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            g.add(new ArrayList<>());
        }
        addEdge(g, 0, 1, 2);
        addEdge(g, 0, 2, 3);
        addEdge(g, 1, 2, 1);
        addEdge(g, 1, 3, 4);
        addEdge(g, 2, 3, 5);
        System.out.println(prim(n, g));
    }

    private static void addEdge(List<List<int[]>> g, int u, int v, int w) {
        g.get(u).add(new int[] {v, w});
        g.get(v).add(new int[] {u, w});
    }
}
