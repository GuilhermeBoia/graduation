import heapq

n, k = map(int, input().split())
horarios = []
for _ in range(n):
    init, end = map(int, input().split())
    horarios.append((init, end))
    
horarios.sort(key=lambda x: x[1])
end_times = [] 
filmes = 0
for init, end in horarios:
    if end_times and end_times[0] <= init:
        heapq.heappop(end_times)
        heapq.heappush(end_times, end)
        filmes += 1
    elif len(end_times) < k:
        heapq.heappush(end_times, end)
        filmes += 1
        
print(filmes)