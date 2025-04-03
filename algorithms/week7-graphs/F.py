import sys
sys.setrecursionlimit(1000000)

def find_cycle(node, parent, graph, visited, path):
    visited[node] = True
    path.append(node)
    
    for neighbor in graph[node]:
        if not visited[neighbor]:
            if find_cycle(neighbor, node, graph, visited, path):
                return True
        elif neighbor != parent:
            path.append(neighbor)
            return True
    
    path.pop()
    return False

n, m = map(int, input().split())
graph = [[] for _ in range(n + 1)]

for _ in range(m):
    a, b = map(int, input().split())
    graph[a].append(b)
    graph[b].append(a)

visited = [False] * (n + 1)
path = []

for start in range(1, n + 1):
    if not visited[start]:
        if find_cycle(start, -1, graph, visited, path):
            cycle_start = path[-1]
            cycle = []
            for node in reversed(path):
                cycle.append(node)
                if node == cycle_start and len(cycle) > 1:
                    break
            cycle.reverse()
            print(len(cycle))
            print(' '.join(map(str, cycle)))
            break
else:
    print("IMPOSSIBLE")