from collections import deque, defaultdict
def encontra_caminho(n, prerequisites):
    graph = defaultdict(list)
    in_degree = [0] * (n + 1)
    for a, b in prerequisites:
        graph[a].append(b)
        in_degree[b] += 1
    queue = deque([i for i in range(1, n + 1) if in_degree[i] == 0])
    order = []
    while queue:
        node = queue.popleft()
        order.append(node)
        for neighbor in graph[node]:
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                queue.append(neighbor)
    if len(order) == n:
        return order
    else:
        return "IMPOSSIBLE"

n, m = map(int, input().split())
prerequisites = [tuple(map(int, input().split())) for _ in range(m)]
result = encontra_caminho(n, prerequisites)

if result == "IMPOSSIBLE":
    print(result)
else:
    print(" ".join(map(str, result)))