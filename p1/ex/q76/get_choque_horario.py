# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Choque de Horário

def meu_count(elemento, lista):
    count = 0
    for e in lista:
        if e == elemento:
            count +=1
    return count

def meu_in(elemento, lista):
    for e in lista:
        if e == elemento:
            return True
    return False

def get_choque_horario(lista):
    periodos = []
    for e in lista:
        periodo = e.split('-')
        periodos.append(periodo[1])

    for i in range(len(periodos) - 1, -1, -1):
        if meu_count(periodos[i], periodos) == 1:
            periodos.pop(i)
    choque = []
    for e1 in periodos:
        if not meu_in(e1, choque):
            choque.append(e1)

    horario = []
    for e2 in choque:
        for j in range(len(lista)):
            termo = lista[j].split('-')
            if termo[1] == e2:
                horario.append(lista[j])

    return horario
