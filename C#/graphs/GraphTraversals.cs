using System;
using System.Collections.Generic;

/// <summary>
/// Graph traversals using adjacency list: BFS and DFS.
/// </summary>
public static class GraphTraversals
{
    public static Dictionary<int, List<int>> BuildUndirected(int n, int[][] edges)
    {
        var graph = new Dictionary<int, List<int>>();
        for (int i = 0; i < n; i++) graph[i] = new List<int>();
        foreach (var e in edges)
        {
            int u = e[0], v = e[1];
            graph[u].Add(v);
            graph[v].Add(u);
        }
        return graph;
    }

    // Time: O(V + E), Space: O(V)
    public static List<int> Bfs(Dictionary<int, List<int>> graph, int start)
    {
        var order = new List<int>();
        var visited = new HashSet<int>();
        var q = new System.Collections.Generic.Queue<int>();
        visited.Add(start);
        q.Enqueue(start);
        while (q.Count > 0)
        {
            int u = q.Dequeue();
            order.Add(u);
            foreach (var v in graph[u])
            {
                if (!visited.Contains(v))
                {
                    visited.Add(v);
                    q.Enqueue(v);
                }
            }
        }
        return order;
    }

    // Time: O(V + E), Space: O(V)
    public static List<int> Dfs(Dictionary<int, List<int>> graph, int start)
    {
        var order = new List<int>();
        var visited = new HashSet<int>();
        void DfsVisit(int u)
        {
            visited.Add(u);
            order.Add(u);
            foreach (var v in graph[u])
            {
                if (!visited.Contains(v)) DfsVisit(v);
            }
        }
        DfsVisit(start);
        return order;
    }

    public static void Main()
    {
        int n = 6;
        int[][] edges = new int[][]
        {
            new[]{0,1}, new[]{0,2}, new[]{1,3}, new[]{2,3}, new[]{3,4}, new[]{4,5}
        };
        var g = BuildUndirected(n, edges);
        Console.WriteLine("[Graph] BFS: " + string.Join(", ", Bfs(g, 0)));
        Console.WriteLine("[Graph] DFS: " + string.Join(", ", Dfs(g, 0)));
    }
}


