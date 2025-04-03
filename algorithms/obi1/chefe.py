n = int(input())

numeros = []
for i in range(n):
    num = int(input())
    if num == 0:
        numeros.pop(len(numeros)-1)
    else:
        numeros.append(num)

soma = sum(numeros)

print(soma)