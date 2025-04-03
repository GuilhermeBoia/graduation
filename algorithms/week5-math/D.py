from math import sqrt

def divisores(n):
    divide = []
    for i in range(1, int(sqrt(n)) + 1):
        if n % i == 0:
            divide.append(i)
            if i != n // i:
                divide.append(n // i)
    
    divide.sort()
    return divide

n, k = map(int, input().split())
if k == 1:
    print(1)
    exit()

if k >= n and n != 2:
    print(-1)
    exit()
else:
    divisor = divisores(n)
    if k > len(divisor):
        print(-1)
    else:
        print(divisor[k - 1])