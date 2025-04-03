# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Elimina Menores

def elimina_menores(num, lista):

    n = 0
    for i in range(len(lista) - 1, -1, -1):
        if num > lista[i]:
            lista.pop(i)
            n += 1

    return n
