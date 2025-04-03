# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Remove divisores


def remove_divisores_k(lista, k, n):
    removeu = 0
    for i in range(len(lista) -1, -1, -1):
        if k % lista[i] == 0:
            if removeu < n:
                lista.pop(i)
                removeu += 1
