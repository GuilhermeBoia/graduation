from math import sqrt

def primeFactors(n):
    prime = set()
    while n % 2 == 0:
        prime.add(2)
        n = n / 2  
    for i in range(3, int(sqrt(n)) + 1, 2):
        while n % i == 0:
            prime.add(int(i))
            n = n / i  
    if n > 2:
        prime.add(int(n))

    return list(prime)

n, m = map(int, input().split())
array = list(map(int, input().split()))

primeF = set()
for f in array:
    primes = primeFactors(f)
    for e in primes:
        primeF.add(e)

is_coprime = [True] * (m + 1)
for e in primeF:
    for i in range(e, m + 2, e):
        is_coprime[i] = False

coprimes = [i for i in range(1, m + 1) if is_coprime[i]]

print(len(coprimes))
for e in coprimes:
    print(e)