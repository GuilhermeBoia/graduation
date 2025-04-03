# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1

def triplets(lista):
    i = len(lista) - 1
    while i > 0:
        triplicado = False
        if count(lista[i], lista) == 3:
            triplicado = True
            numero = lista[i]
            for j in range(i, -1, -1):
                if numero == lista[j]:
                    lista.pop(j)
                    i -= 1
        else:
            i -= 1

def count(elemento, sequencia):
    n = 0
    for e in sequencia:
        if e == elemento:
            n += 1
    return n
