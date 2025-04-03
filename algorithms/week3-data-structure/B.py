n = int(input())
array = list(map(int, input().split()))

inicio = 0
unicos = set()
max_final = 0
for fim in range(n):
    
    while array[fim] in unicos:
        unicos.remove(array[inicio])
        inicio += 1

    unicos.add(array[fim])
    max_final = max(max_final, fim - inicio + 1)

print(max_final)