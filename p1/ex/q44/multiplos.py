# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Soma de múltiplos

n = int(input())

soma = 0
for i in range(10):
    num = int(input())
    if num % n == 0:
        soma += num

print(soma)
