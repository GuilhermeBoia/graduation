# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Ajusta Lista


def ajusta_lista(lista):
    for i in range(len(lista) -1, -1, -1):
        if len(lista[i]) % 3 == 0 and len(lista[i]) % 4 == 0:
            lista[i] += "12"

        elif len(lista[i]) % 4 == 0 and len(lista[i]) % 5 == 0:
            temp = lista[i]
            lista[i] = "20"
            lista[i] += temp

        elif len(lista[i]) % 3 == 0 and len(lista[i]) % 5 == 0:
            lista[i] = "sessenta"

        elif len(lista[i]) % 3 == 0 and len(lista[i]) % 4 != 0 and len(lista[i]) % 5 != 0:
            lista.pop(i)
        elif len(lista[i]) % 3 != 0 and len(lista[i]) % 4 == 0 and len(lista[i]) % 5 != 0:
            lista.pop(i)
        elif len(lista[i]) % 3 != 0 and len(lista[i]) % 4 != 0 and len(lista[i]) % 5 == 0:
            lista.pop(i)
