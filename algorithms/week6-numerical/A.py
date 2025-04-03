def pot(a, b, m):
    a %= m
    res = 1
    while (b > 0):
        if (b & 1):
            res = res * a % m
        a = a * a % m
        b >>= 1
    return res

n = int(input())
for i in range(n):
    a, b = map(int, input().split())
    modulo = (1000000000 + 7)
    numero = pot(a, b, modulo)

    resultado = numero % (1000000000 + 7)
    print(resultado)