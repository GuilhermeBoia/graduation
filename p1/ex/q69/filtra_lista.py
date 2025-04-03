# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Filtra Elementos de uma Lista

def filtra_lista(num, numeros):
    lista = []
    for i in range(len(numeros)):
        if i % num == 0:
            lista.append(numeros[i])
    
    return lista
