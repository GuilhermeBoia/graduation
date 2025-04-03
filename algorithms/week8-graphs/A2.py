import heapq
from math import ceil
def find_inn():
    # Recebendo input do usuário
    N, M, T, K, P = map(int, input().split())
    pine_tree_areas = list(map(int, input().split()))
    paths = []
    for _ in range(M):
        x, y, w = map(int, input().split())
        paths.append((x, y, w))

    # Criando a representação do grafo
    graph = [[] for _ in range(N + 1)]
    for x, y, w in paths:
        graph[x].append((y, w))
    
    pine_trees = set(pine_tree_areas)  # Áreas com pinheiros

    # Implementação do algoritmo de Dijkstra
    def dijkstra():
        pq = [(0, 1)]  # Fila de prioridade iniciando da área 1
        dist = [float('inf')] * (N + 1)
        dist[1] = 0
        
        while pq:
            curr_time, area = heapq.heappop(pq)
            
            if curr_time > dist[area]:
                continue
                
            # Percorrendo vizinhos
            for neighbor, travel_time in graph[area]:
                new_time = curr_time + travel_time
                # Adiciona tempo adicional se houver pinheiro
                if neighbor in pine_trees:
                    new_time += K / 60.0  # Convertendo segundos para minutos
                
                if new_time < dist[neighbor]:
                    dist[neighbor] = new_time
                    heapq.heappush(pq, (new_time, neighbor))
        
        return dist[N]
    
    # Executa o Dijkstra
    result = dijkstra()
    
    # Verifica se o resultado está dentro do limite de tempo
    if result <= T:
        print(ceil(int(result * 60)))  # Converte de volta para segundos
    else:
        print(-1)

# Chamando a função para rodar o algoritmo
find_inn()