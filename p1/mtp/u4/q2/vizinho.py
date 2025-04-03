# Guilherme Dantas Boia de Albuquerque
# Números vizinhos iguais

quantidade = int(input())

numeros = []
for i in range(quantidade):
    num = int(input())
    numeros.append(num)


for j in range(len(numeros)- 1):
    if numeros[j] == numeros[j+1]:
        numeros[j+1] = numeros[j+1] - 1

count = 0
for k in range(len(numeros)):
    if count == len(numeros) - 1:
        print(numeros[k])
    else:
        print(numeros[k], end=" ")
        count += 1
