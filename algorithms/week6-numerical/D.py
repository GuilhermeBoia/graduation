def pot(a, b, m):
    a %= m
    res = 1
    while (b > 0):
        if (b & 1):
            res = res * a % m
        a = a * a % m
        b >>= 1
    return res

def soma(n, k, modulo):
    sn = pot(n, k, modulo)
    pn = pot(n, n, modulo)
    return (sn + pn) % modulo

while True:
    n, k = map(int, input().split())
    if n == 0 and k == 0:
        exit()

    modulo = 10000007

    zn, zn1, zn2 = 0, 0, 0
    for i in range(1, n + 1):
        current_sum = soma(i, k, modulo)
        zn = (zn + current_sum) % modulo
        if i <= n - 1:
            zn1 = (zn1 + current_sum) % modulo
        if i <= n - 2:
            zn2 = (zn2 + current_sum) % modulo

    total = (zn + zn1 - 2 * zn2) % modulo

    print(total)