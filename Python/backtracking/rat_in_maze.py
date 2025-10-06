def find_all_paths(maze, n):
    """
    Rat in a Maze - Find all paths from (0,0) to (n-1,n-1)
    Uses backtracking to explore all possible paths
    Time Complexity: O(4^(n²)) in worst case
    Space Complexity: O(n²) for visited array + O(n²) recursion depth
    
    Movement directions: D (Down), L (Left), R (Right), U (Up)
    """
    
    def is_safe(x, y, visited):
        """Check if position is valid and unvisited"""
        return (0 <= x < n and 
                0 <= y < n and 
                maze[x][y] == 1 and 
                not visited[x][y])
    
    def dfs(x, y, path, visited):
        """DFS to explore all paths with backtracking"""
        # Base case: reached destination
        if x == n - 1 and y == n - 1:
            result.append(path)
            return
        
        # Explore in lexicographic order: D, L, R, U
        directions = ['D', 'L', 'R', 'U']
        dx = [1, 0, 0, -1]  # Row changes
        dy = [0, -1, 1, 0]  # Column changes
        
        for i in range(4):
            nx, ny = x + dx[i], y + dy[i]
            
            if is_safe(nx, ny, visited):
                # Mark as visited
                visited[nx][ny] = True
                
                # Explore this path
                dfs(nx, ny, path + directions[i], visited)
                
                # Backtrack: unmark for other paths
                visited[nx][ny] = False
    
    result = []
    
    # Edge case: start or end blocked
    if not maze or maze[0][0] == 0 or maze[n - 1][n - 1] == 0:
        return ["-1"]
    
    # Initialize visited array
    visited = [[False] * n for _ in range(n)]
    visited[0][0] = True
    
    # Start DFS from (0, 0)
    dfs(0, 0, "", visited)
    
    return sorted(result) if result else ["-1"]


def find_shortest_path(maze, n):
    """
    Find the shortest path using BFS
    Time Complexity: O(n²)
    Space Complexity: O(n²)
    """
    from collections import deque
    
    if not maze or maze[0][0] == 0 or maze[n - 1][n - 1] == 0:
        return "-1"
    
    # BFS setup
    queue = deque([(0, 0, "")])  # (x, y, path)
    visited = [[False] * n for _ in range(n)]
    visited[0][0] = True
    
    # Direction mappings
    directions = ['D', 'L', 'R', 'U']
    dx = [1, 0, 0, -1]
    dy = [0, -1, 1, 0]
    
    while queue:
        x, y, path = queue.popleft()
        
        # Reached destination
        if x == n - 1 and y == n - 1:
            return path
        
        # Explore neighbors
        for i in range(4):
            nx, ny = x + dx[i], y + dy[i]
            
            if (0 <= nx < n and 0 <= ny < n and 
                maze[nx][ny] == 1 and not visited[nx][ny]):
                visited[nx][ny] = True
                queue.append((nx, ny, path + directions[i]))
    
    return "-1"


def find_path_with_coordinates(maze, n):
    """
    Returns paths with coordinate representation
    """
    
    def dfs(x, y, path, coords, visited):
        if x == n - 1 and y == n - 1:
            result.append((path, coords[:]))
            return
        
        directions = ['D', 'L', 'R', 'U']
        dx = [1, 0, 0, -1]
        dy = [0, -1, 1, 0]
        
        for i in range(4):
            nx, ny = x + dx[i], y + dy[i]
            
            if (0 <= nx < n and 0 <= ny < n and 
                maze[nx][ny] == 1 and not visited[nx][ny]):
                visited[nx][ny] = True
                coords.append((nx, ny))
                
                dfs(nx, ny, path + directions[i], coords, visited)
                
                coords.pop()
                visited[nx][ny] = False
    
    result = []
    
    if not maze or maze[0][0] == 0 or maze[n - 1][n - 1] == 0:
        return []
    
    visited = [[False] * n for _ in range(n)]
    visited[0][0] = True
    
    dfs(0, 0, "", [(0, 0)], visited)
    
    return result


def count_all_paths(maze, n):
    """
    Count total number of paths without storing them
    More memory efficient for large mazes
    """
    
    def dfs(x, y, visited):
        if x == n - 1 and y == n - 1:
            return 1
        
        count = 0
        dx = [1, 0, 0, -1]
        dy = [0, -1, 1, 0]
        
        for i in range(4):
            nx, ny = x + dx[i], y + dy[i]
            
            if (0 <= nx < n and 0 <= ny < n and 
                maze[nx][ny] == 1 and not visited[nx][ny]):
                visited[nx][ny] = True
                count += dfs(nx, ny, visited)
                visited[nx][ny] = False
        
        return count
    
    if not maze or maze[0][0] == 0 or maze[n - 1][n - 1] == 0:
        return 0
    
    visited = [[False] * n for _ in range(n)]
    visited[0][0] = True
    
    return dfs(0, 0, visited)


def visualize_path(maze, n, path):
    """
    Visualize a path on the maze
    """
    # Create a copy of maze for visualization
    visual = [row[:] for row in maze]
    
    x, y = 0, 0
    visual[x][y] = 'S'  # Start
    
    direction_map = {
        'D': (1, 0),
        'L': (0, -1),
        'R': (0, 1),
        'U': (-1, 0)
    }
    
    for move in path:
        dx, dy = direction_map[move]
        x, y = x + dx, y + dy
        if 0 <= x < n and 0 <= y < n:
            visual[x][y] = move
    
    visual[n-1][n-1] = 'E'  # End
    
    return visual


# Test the implementations
if __name__ == "__main__":
    # Test cases
    test_cases = [
        {
            'maze': [
                [1, 0, 0, 0],
                [1, 1, 0, 1],
                [1, 1, 0, 0],
                [0, 1, 1, 1]
            ],
            'n': 4,
            'description': 'Standard maze with multiple paths'
        },
        {
            'maze': [
                [1, 1, 1],
                [1, 1, 1],
                [1, 1, 1]
            ],
            'n': 3,
            'description': 'All cells open - many paths'
        },
        {
            'maze': [
                [1, 0],
                [1, 1]
            ],
            'n': 2,
            'description': 'Small maze'
        },
        {
            'maze': [
                [1, 1, 0],
                [0, 1, 1],
                [0, 0, 1]
            ],
            'n': 3,
            'description': 'Single path maze'
        },
        {
            'maze': [
                [1, 0],
                [0, 1]
            ],
            'n': 2,
            'description': 'No path possible'
        },
    ]
    
    print("=" * 70)
    print("RAT IN A MAZE - ALL PATHS")
    print("=" * 70)
    
    for i, test in enumerate(test_cases, 1):
        maze = test['maze']
        n = test['n']
        
        print(f"\nTest Case {i}: {test['description']}")
        print("Maze:")
        for row in maze:
            print("  ", row)
        
        # Find all paths
        all_paths = find_all_paths(maze, n)
        shortest_path = find_shortest_path(maze, n)
        path_count = count_all_paths(maze, n)
        
        print(f"\nAll paths (sorted):        {all_paths}")
        print(f"Shortest path:             {shortest_path}")
        print(f"Total number of paths:     {path_count}")
        
        # Visualize first path if exists
        if all_paths != ["-1"] and all_paths:
            print(f"\nVisualizing path: {all_paths[0]}")
            visual = visualize_path(maze, n, all_paths[0])
            for row in visual:
                print("  ", row)
    
    # Test with coordinates
    print("\n" + "=" * 70)
    print("PATH WITH COORDINATES")
    print("=" * 70)
    
    maze = [
        [1, 1, 0],
        [1, 1, 1],
        [0, 1, 1]
    ]
    n = 3
    
    print("\nMaze:")
    for row in maze:
        print("  ", row)
    
    paths_with_coords = find_path_with_coordinates(maze, n)
    
    print(f"\nFound {len(paths_with_coords)} path(s):")
    for i, (path, coords) in enumerate(paths_with_coords, 1):
        print(f"\nPath {i}: {path}")
        print(f"Coordinates: {' -> '.join(str(c) for c in coords)}")
    
    # Performance test
    import time
    
    print("\n" + "=" * 70)
    print("PERFORMANCE TEST")
    print("=" * 70)
    
    # Create a larger maze
    large_n = 8
    large_maze = [[1] * large_n for _ in range(large_n)]
    
    # Add some obstacles
    for i in range(1, large_n - 1):
        large_maze[i][i] = 0
    
    print(f"\nMaze size: {large_n}x{large_n}")
    
    start = time.time()
    path_count = count_all_paths(large_maze, large_n)
    time_count = (time.time() - start) * 1000
    
    start = time.time()
    shortest = find_shortest_path(large_maze, large_n)
    time_shortest = (time.time() - start) * 1000
    
    print(f"Total paths:               {path_count}")
    print(f"Shortest path length:      {len(shortest) if shortest != '-1' else 'N/A'}")
    print(f"\nTime to count paths:       {time_count:.2f}ms")
    print(f"Time to find shortest:     {time_shortest:.2f}ms")