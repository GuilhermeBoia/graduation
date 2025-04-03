# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Blefe

def blefe(lista):
    compara = []
    for e in lista:
        compara.append(e)

    for j in range(len(lista) -1):
        if lista[j] < lista[j+1]:
            lista[j+1] = lista[j]

    diferenca = []
    for i in range(len(compara)):
        dif = compara[i] - lista[i]
        diferenca.append(dif)

    return diferenca
