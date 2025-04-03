# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Soma interval0

def soma_intervalo(a, b):
    soma = 0
    for i in range(a, b + 1):
        soma += i

    return soma
