import heapq
import sys

INF = 10**16
def dijkstra(start, n, adj):
    dist = [INF] * (n + 1)
    dist[start] = 0
    visited = [False] * (n + 1)
    pq = [(0, start)]

    while pq:
        current_dist, node = heapq.heappop(pq)

        if visited[node]:
            continue

        visited[node] = True

        for neighbor, weight in adj[node]:
            if dist[node] + weight < dist[neighbor]:
                dist[neighbor] = dist[node] + weight
                heapq.heappush(pq, (dist[neighbor], neighbor))

    return dist

def solve():
    input = sys.stdin.read
    data = input().split()
    index = 0

    n = int(data[index])
    m = int(data[index + 1])
    index += 2

    adj1 = [[] for _ in range(n + 1)]
    adj2 = [[] for _ in range(n + 1)]
    edges = []

    for _ in range(m):
        a = int(data[index])
        b = int(data[index + 1])
        w = int(data[index + 2])
        index += 3

        edges.append((a, b, w))
        adj1[a].append((b, w))
        adj2[b].append((a, w))

    dis1 = dijkstra(1, n, adj1)
    dis2 = dijkstra(n, n, adj2)

    result = INF

    for a, b, w in edges:
        if dis1[a] != INF and dis2[b] != INF:
            result = min(result, dis1[a] + dis2[b] + w // 2)

    print(result)

if __name__ == "__main__":
    solve()