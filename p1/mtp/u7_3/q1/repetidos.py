# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Repetidos

def meu_in(elemento, lista):
    tem = False
    for e in lista:
        if e == elemento:
            tem = True
    return tem

def meu_count(elemento, lista):
    count = 0
    for e in lista:
        if e == elemento:
            count += 1
    return count


def repetidos(sequencia):
    lista = []
    for e in sequencia:
        if meu_count(e, sequencia) > 1:
            if not meu_in(e, lista):
                lista.append(e)

    return lista
