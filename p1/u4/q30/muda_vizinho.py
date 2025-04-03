# Guilherme Dantas Boia de Albuquerque
# Muda o número vizinho

linhas = int(input())
inteiros = []
i = 0
while i <= linhas - 1:
    numero = int(input())
    inteiros.append(numero)
    i += 1


j = 0 
lista = []
while j <= len(inteiros) - 2:
    if inteiros[j] == inteiros[j+1]:
        inteiros [j+1] -= 1
    j += 1


k = 0
while True:
    if k == len(inteiros) - 1:
        print(inteiros[k])
        break
    else:
        print(inteiros[k], end=" ")
    k += 1
