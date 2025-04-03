from collections import deque;

n, q = map(int, input().split())
numbers = deque(map(int, input().split()))

for _ in range(q):
    query = int(input())
    reply = numbers.copy()

    for i in range(len(reply)):

        first = reply.popleft()
        if first == max(numbers):
            reply.appendleft(first)
            modulo = query % (len(reply) - 1)
            if modulo == 0:
                modulo = len(reply) - 1
            second = reply[modulo]
            break

        second = reply.popleft()

        if first > second:
            reply.appendleft(first)
            reply.append(second)
        else:
            reply.appendleft(second)
            reply.append(first)
        
    print(f"{first} {second}")