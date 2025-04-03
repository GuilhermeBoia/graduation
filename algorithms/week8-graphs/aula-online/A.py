from collections import defaultdict
import heapq

def traffic_light(n, m, t, edges):
    graph = defaultdict(list)
    for a, b, x, y in edges:
        graph[a].append((b, x, y)) 
        graph[b].append((a, x, y)) 

    distances = [float('inf')] * (n + 1)
    distances[1] = 0
    queue = [(0, 1)]  

    while queue:
        tempo, encontro = heapq.heappop(queue)

        if encontro == n:
            return tempo

        for vizinho, green, red in graph[encontro]:
            ciclo = green + red
            last_green = (tempo + distances[encontro]) % ciclo # corrected line
            if last_green >= green:
                espera = ciclo - last_green
            else:
                espera = 0

            chegada = tempo + espera + 1 

            if chegada < distances[vizinho]:
                distances[vizinho] = chegada
                heapq.heappush(queue, (chegada, vizinho))

    return -1 

n, m, t = map(int, input().split())
edges = []
for _ in range(m):
    tupla = tuple(map(int, input().split()))
    edges.append(tupla)

menor = traffic_light(n, m, t, edges)
print(menor) 