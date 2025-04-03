def pot(a, b, m):
    a %= m
    res = 1
    while b > 0:
        if b & 1:
            res = (res * a) % m
        a = (a * a) % m
        b >>= 1
    return res


m = 10000007
while True:
    n, k = map(int, input().split())
    if n == 0 and k == 0:
        break
    res = (pot(n, k, m) + 2 * pot(n - 1, k, m) +
           pot(n, n, m) + 2 * pot(n - 1, n - 1, m))
    print(res % m)