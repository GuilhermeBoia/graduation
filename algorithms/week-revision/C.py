from math import sqrt

def crivo(n):
    is_prime = [True] * (n + 1)
    is_prime[0] = is_prime[1] = False
    p = 2
    while (p * p <= n):
        if (is_prime[p] == True):
            for i in range(p * p, n + 1, p):
                is_prime[i] = False
        p += 1
    primes = [p for p in range(n + 1) if is_prime[p]]
    return primes

def t_primes(n):
    primes = crivo(int(sqrt(n)))
    t_primes = set([p * p for p in primes])
    return t_primes

n = int(input())
array = list(map(int, input().split()))

t_primos = t_primes(10 ** 12)
for e in array:
    if e in t_primos:
        print('YES')
    else:
        print('NO')