from collections import deque

def bfs(start, n, tree):
    distances = [-1] * (n + 1)
    queue = deque([start])
    distances[start] = 0
    farthest_node = start
    max_distance = 0
    
    while queue:
        node = queue.popleft()
        current_distance = distances[node]
        
        for neighbor in tree[node]:
            if distances[neighbor] == -1:
                distances[neighbor] = current_distance + 1
                queue.append(neighbor)
                if distances[neighbor] > max_distance:
                    max_distance = distances[neighbor]
                    farthest_node = neighbor
    
    return farthest_node, distances

n = int(input())
tree = [[] for _ in range(n + 1)]
for _ in range(n - 1):
    a, b = map(int, input().split())
    tree[a].append(b)
    tree[b].append(a)

node1, _ = bfs(1, n, tree)
node2, dist1 = bfs(node1, n, tree)
_, dist2 = bfs(node2, n, tree)

results = [max(dist1[i], dist2[i]) for i in range(1, n + 1)]

print(' '.join(map(str, results)))