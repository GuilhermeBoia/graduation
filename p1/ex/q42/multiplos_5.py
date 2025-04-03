# Guilherme Dantas Boia de Albuquerque
# Prog1 | UFCG - 2022.1
# Múltiplos de 5 pares


limite = int(input())

i = 1
while True:
    if (5 * i) >= limite: break

    if (5 * i) % 2 == 0:
        print(5 * i)

    i += 1
