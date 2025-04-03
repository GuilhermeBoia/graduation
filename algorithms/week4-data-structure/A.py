from queue import PriorityQueue

t = int(input())

for i in range(t):
    stack = PriorityQueue()
    n = int(input())
    array = list(map(int, input().split()))
    powers = []
    for j in range(n):
        if array[j] == 0:
            if not stack.empty():
                powers.append(-stack.get())
        else:
            stack.put(-array[j])
        
    result = 0
    for e in powers:
        result += e

    print(result)