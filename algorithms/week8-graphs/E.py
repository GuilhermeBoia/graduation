import heapq
import sys

# Constants
MAXN = 100005
INF = float('inf')

def dijkstra(s, t, n, adj_list):
    dist = [INF] * (n + 1)
    par = [-1] * (n + 1)
    is_done = [False] * (n + 1)

    pq = []
    heapq.heappush(pq, (0, s))
    dist[s] = 0

    while pq:
        current_dist, u = heapq.heappop(pq)

        if u == t:
            return True, par

        if is_done[u]:
            continue

        is_done[u] = True

        for v, w in adj_list[u]:
            if not is_done[v] and dist[u] + w < dist[v]:
                dist[v] = dist[u] + w
                heapq.heappush(pq, (dist[v], v))
                par[v] = u

    return False, par

def main():
    input = sys.stdin.read
    data = input().split()
    index = 0

    n = int(data[index])
    m = int(data[index + 1])
    index += 2

    adj_list = [[] for _ in range(n + 1)]

    for _ in range(m):
        u = int(data[index])
        v = int(data[index + 1])
        w = int(data[index + 2])
        index += 3

        adj_list[u].append((v, w))
        adj_list[v].append((u, w))

    found, par = dijkstra(1, n, n, adj_list)

    if found:
        path = []
        v = n
        while v != -1:
            path.append(v)
            v = par[v]

        path.reverse()
        print(' '.join(map(str, path)))
    else:
        print(-1)

if __name__ == "__main__":
    main()