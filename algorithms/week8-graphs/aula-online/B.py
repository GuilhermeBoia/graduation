n = int(input())
qualif = list(map(int, input().split()))
m = int(input())

custo = [0] * (n + 1)
check = [False] * (n + 1)
total = 0
subordinados = 0
for _ in range(m):
    a, b, c = map(int, input().split())
    if not check[b]:
        subordinados += 1
        total += c
        custo[b] = c
        check[b] = True
    else:
        if custo[b] > c:
            total = total - custo[b] + c
            custo[b] = c

if subordinados == n - 1:
    print(total)
else:
    print(-1)