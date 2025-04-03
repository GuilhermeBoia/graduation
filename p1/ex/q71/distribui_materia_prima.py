# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Distribui Matéria Prima

def distribui_materia_prima(esteira, n):
    dividido = []
    for _ in range(n):
        lista = []
        dividido.append(lista)

    count = 0
    for i in range(len(esteira)):
        dividido[count].append(esteira[i])
        count += 1
        if count == n:
            count = 0

    return dividido
