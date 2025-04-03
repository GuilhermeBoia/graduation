import math


def mmc(a, b):
    maior = max(a, b)
    menor = min(a, b)
    for i in range(maior, a*b+1, maior):
        if i % menor == 0:
            return i


n = int(input())

for i in range(n):
    number = int(input())
