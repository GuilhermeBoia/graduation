import heapq

t = int(input())

for _ in range(t):
    n = int(input())
    sociability = list(map(int, input().split()))
    conversations = []
    heap = []
    for i in range(n):
        if sociability[i] > 0:
            heapq.heappush(heap, (-sociability[i], i + 1))
    
    while len(heap) > 1:
        soc1, person1 = heapq.heappop(heap)
        soc2, person2 = heapq.heappop(heap)
        conversations.append((person1, person2))
        if soc1 + 1 < 0:
            heapq.heappush(heap, (soc1 + 1, person1))
        if soc2 + 1 < 0:
            heapq.heappush(heap, (soc2 + 1, person2))
    
    print(len(conversations))
    for conversation in conversations:
        print(conversation[0], conversation[1])