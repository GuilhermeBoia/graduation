def digits(n):
    digitos = []
    while n > 0:
        digitos.append(n % 10)
        n = n // 10
        
    return digitos

n = int(input())

step = 0
while n > 0:
    maximo = max(digits(n))
    n -= maximo
    step += 1

print(step)