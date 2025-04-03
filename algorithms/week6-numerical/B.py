def is_valid(x, n):
    novo_a = [(a[i] + x) % m for i in range(n)]
    novo_a.sort()
    return novo_a == b

n, m = map(int, input().split())
a = list(map(int, input().split()))
b = list(map(int, input().split()))

a.sort()
b.sort()
min_x = float('inf')
for i in range(n):
    x = (b[0] - a[i] + m) % m
    if is_valid(x, n):
        min_x = min(min_x, x)

print(min_x)