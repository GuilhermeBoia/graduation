# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Make Set

def meu_count(elemento, lista):
    count = 0
    for e in lista:
        if e == elemento:
            count += 1
    return count

def make_set(lista):
    for i in range(len(lista) - 1, -1, -1):
        if meu_count(lista[i], lista) > 1:
            lista.pop(i)
