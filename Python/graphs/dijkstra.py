"""Dijkstra's algorithm — single-file implementation with CLI input.

This file contains a clean, optimal implementation of Dijkstra's algorithm
and a small command-line parser so users can run test cases from stdin.

Input format (stdin):
- Single test case:
    n m s t
    u1 v1 w1
    u2 v2 w2
    ... (m edges)

- Multiple test cases:
    T
    n m s t
    (m edges)
    n m s t
    (m edges)

Notes on indexing: nodes are 0-based (0..n-1). If you prefer 1-based input
convert inputs before calling the functions.

If t is -1 the program will print all shortest distances from s. Otherwise it
prints the shortest distance to t and the reconstructed path (if reachable).

Functions exported:
- dijkstra(graph, source) -> (distances, parents)
- shortest_path(graph, source, target) -> (distance, path)

Complexity
- Time: O((V + E) log V) using a binary heap.
- Space: O(V).
"""

from typing import List, Tuple, Optional
import heapq
import math
import sys


Graph = List[List[Tuple[int, float]]]


def dijkstra(graph: Graph, source: int) -> Tuple[List[float], List[Optional[int]]]:
    """Compute shortest distances and parent pointers from `source`.

    Args:
        graph: adjacency list where graph[u] is list of (v, weight)
        source: starting node (0-based)

    Returns:
        distances: list of length n with shortest distances (math.inf if unreachable)
        parents: list of parent pointers for path reconstruction
    """
    n = len(graph)
    if source < 0 or source >= n:
        raise IndexError("source index out of bounds")

    dist: List[float] = [math.inf] * n
    parent: List[Optional[int]] = [None] * n
    dist[source] = 0.0

    heap: List[Tuple[float, int]] = [(0.0, source)]

    while heap:
        d, u = heapq.heappop(heap)
        # skip stale entry
        if d > dist[u]:
            continue

        for v, w in graph[u]:
            if w < 0:
                raise ValueError("Dijkstra's algorithm does not support negative weights")
            nd = d + w
            if nd < dist[v]:
                dist[v] = nd
                parent[v] = u
                heapq.heappush(heap, (nd, v))

    return dist, parent


def shortest_path(graph: Graph, source: int, target: int) -> Tuple[float, List[int]]:
    """Return (distance, path) from source to target. Path empty if unreachable."""
    dist, parent = dijkstra(graph, source)
    n = len(graph)
    if target < 0 or target >= n:
        raise IndexError("target index out of bounds")
    if dist[target] == math.inf:
        return math.inf, []

    path: List[int] = []
    cur: Optional[int] = target
    while cur is not None:
        path.append(cur)
        cur = parent[cur]
    path.reverse()
    return dist[target], path


def parse_ints(tokens: List[str]) -> List[int]:
    return [int(x) for x in tokens]


def run_from_stdin() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        print("No input detected. Run the file without stdin to see an example.")
        return

    it = iter(data)

    def next_int() -> int:
        return int(next(it))

    # detect if first token is number of test cases
    tokens = list(map(int, data))
    pos = 0
    t = 1
    if len(tokens) >= 1:
        # Heuristic: if first value equals remaining-length formatted as tests count
        first = tokens[0]
        # if there's enough data and first seems like T (and not n)
        if first > 0 and 1 + 1 < len(tokens):
            # we'll treat first as T only if the structure matches multiple tests
            # Conservative approach: if interpreting first as T leads to matching counts
            possible_t = first
            # can't reliably validate here without complex checks; accept it as T
            # only when possible_t * 3 <= remaining tokens roughly
            if possible_t * 3 <= len(tokens):
                t = possible_t
                pos = 1

    # simple parser using position index
    idx = pos
    out_lines: List[str] = []
    for _ in range(t):
        if idx + 3 > len(tokens):
            raise ValueError("Insufficient header tokens for test case. Expected n m s t.")
        n = tokens[idx]
        m = tokens[idx + 1]
        s = tokens[idx + 2]
        target = tokens[idx + 3] if idx + 3 < len(tokens) else -1
        idx += 4

        if n <= 0:
            raise ValueError("n must be positive")
        if m < 0:
            raise ValueError("m must be non-negative")

        # build empty adjacency list
        graph: Graph = [[] for _ in range(n)]
        if idx + 3 * m > len(tokens):
            raise ValueError("Not enough edge tokens for declared m")

        for _e in range(m):
            u = tokens[idx]
            v = tokens[idx + 1]
            w = tokens[idx + 2]
            idx += 3
            # assume inputs are 0-based; caller can adjust if needed
            if u < 0 or u >= n or v < 0 or v >= n:
                raise IndexError(f"edge node out of bounds: {u}, {v}")
            graph[u].append((v, w))

        # run
        if target == -1:
            distances, _ = dijkstra(graph, s)
            out_lines.append(" ".join(str(x if x != math.inf else -1) for x in distances))
        else:
            dist, path = shortest_path(graph, s, target)
            if dist == math.inf:
                out_lines.append("-1")
            else:
                out_lines.append(f"{dist} | path: {'->'.join(map(str, path))}")

    print("\n".join(out_lines))


def _example() -> None:
    # Classic example (undirected) used frequently in textbooks
    graph_example: Graph = [
        [(1, 7), (2, 9), (5, 14)],
        [(0, 7), (2, 10), (3, 15)],
        [(0, 9), (1, 10), (3, 11), (5, 2)],
        [(1, 15), (2, 11), (4, 6)],
        [(3, 6), (5, 9)],
        [(0, 14), (2, 2), (4, 9)],
    ]

    src = 0
    distances, parents = dijkstra(graph_example, src)
    print("Distances from {}: {}".format(src, distances))
    target = 4
    d, path = shortest_path(graph_example, src, target)
    print(f"Shortest distance {src} -> {target}: {d}")
    print(f"Path: {path}")


def _run_embedded_tests() -> None:
    """Embedded smoke tests moved from test_dijkstra.py."""
    def test_dijkstra_basic():
        graph = [
            [(1, 7), (2, 9), (5, 14)],
            [(0, 7), (2, 10), (3, 15)],
            [(0, 9), (1, 10), (3, 11), (5, 2)],
            [(1, 15), (2, 11), (4, 6)],
            [(3, 6), (5, 9)],
            [(0, 14), (2, 2), (4, 9)],
        ]
        distances, _ = dijkstra(graph, 0)
        assert distances[0] == 0
        assert distances[1] == 7
        assert distances[2] == 9
        assert distances[5] == 11

    def test_shortest_path():
        graph = [
            [(1, 1), (2, 4)],
            [(2, 2), (3, 6)],
            [(3, 3)],
            [],
        ]
        dist, path = shortest_path(graph, 0, 3)
        assert dist == 6
        assert path == [0, 1, 2, 3]

    test_dijkstra_basic()
    test_shortest_path()
    print("Embedded Dijkstra tests passed.")


if __name__ == "__main__":
    # If stdin has data, parse it as testcases; otherwise show example
    if not sys.stdin.isatty():
        run_from_stdin()
    else:
        _example()


