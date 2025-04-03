from collections import deque

def bfs(start, n, tree):
    dist = [-1] * (n + 1)
    queue = deque([start])
    dist[start] = 0
    far_node = start
    max_dist = 0
    
    while queue:
        node = queue.popleft()
        curr_dist = dist[node]
        
        for neighbor in tree[node]:
            if dist[neighbor] == -1:
                dist[neighbor] = curr_dist + 1
                queue.append(neighbor)
                if dist[neighbor] > max_dist:
                    max_dist = dist[neighbor]
                    far_node = neighbor
    
    return far_node, max_dist

n = int(input())
tree = [[] for _ in range(n + 1)]

for _ in range(n - 1):
    a, b = map(int, input().split())
    tree[a].append(b)
    tree[b].append(a)

node1, _ = bfs(1, n, tree)
node2, diameter = bfs(node1, n, tree)

print(diameter)