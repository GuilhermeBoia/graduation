# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Imprime o número de dígitos de um número

n = int(input())

i = 1
while True:
    if n // 10 == 0:break
    
    if n // 10 > 0:
        i += 1
        n = n // 10

print(i)
