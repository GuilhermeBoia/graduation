from collections import deque;

n, q = map(int, input().split())
numbers = deque(map(int, input().split()))

for _ in range(q):
    reply = numbers.copy()

    respostas = []
    for i in range(len(reply)):
        first = reply.popleft()
        second = reply.popleft()

        if first > second:
            reply.appendleft(first)
            reply.append(second)
        else:
            reply.appendleft(second)
            reply.append(first)
        respostas.append(f"{first} {second}")

    query = int(input())
    if query <= len(respostas):
        print(respostas[query - 1])
    
    else:
        modulo = query % (len(reply))
        if modulo == 0:
            modulo = 1
        print(f"{max(numbers)} {reply[modulo]}")