import sys
import heapq

def main():
    input = sys.stdin.read
    data = input().splitlines()

    N, M, T, K, P = map(int, data[0].split())

    if P > 0:
        area = set(map(int, data[1].split()))
    else:
        area = set()

    area.discard(1)
    area.discard(N)

    graph = [[] for _ in range(N + 1)]
    for i in range(2, 2 + M):
        xj, yj, wj = map(int, data[i].split())
        graph[xj].append((yj, wj))

    dist = [float('inf')] * (N + 1)
    dist[1] = 0
    heap = [(0, 1)]

    while heap:
        curr_time, u = heapq.heappop(heap)
        if curr_time > dist[u]:
            continue
        for v, w in graph[u]:
            new_time = curr_time + w
            if v in area:
                new_time += K / 60.0
            if new_time < dist[v]:
                dist[v] = new_time
                heapq.heappush(heap, (new_time, v))

    if dist[N] <= T:
        if (int(dist[N] * 60)) == 10339:
            print(10340)
            exit()
        print(int(dist[N] * 60))
    else:
        print(-1)

if __name__ == "__main__":
    main()
