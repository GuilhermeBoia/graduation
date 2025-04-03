from  queue import PriorityQueue;

t = int(input())

for _ in range(t):
    n = int(input())
    array = input().split()
    count = 0
    unicos = {}
    for i in range(n):
        num = int(array[i])
        if num not in unicos:
            unicos[num] = 0
            count += 1

        unicos[num] += 1

    q = PriorityQueue(maxsize=n)

    for value in unicos.values():
        q.put((-value, value))

    while count > 1:
        first = (q.get()[1]) - 1
        second = (q.get()[1]) - 1
        if first != 0:
            q.put((-first, first))
        else:
            count -= 1
        if second != 0:
            q.put((-second, second))
        else:
            count -= 1

    if count == 0:
        print(0)
    else:
        print(q.get()[1])