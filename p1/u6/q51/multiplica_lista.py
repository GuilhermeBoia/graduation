# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Multiplica uma lista

def multiplica_lista(n, lista):
    if n == 0:
        return []

    inicial = len(lista)
    i = 0
    nova = []
    while True:
        if i >= n: break
        for j in range(len(lista)):
            if j < inicial:
                nova.append(lista[j])
            else:
                break
        i += 1

    return nova
