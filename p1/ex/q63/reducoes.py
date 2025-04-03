# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Reduções

def reducoes(seq):
    if len(seq) <= 1:
        return []

    lista = []
    for i in range(len(seq) - 1):
        dif = seq[i] - seq[i+1]
        if dif < 1:
            lista.append(0)
        else:
            lista.append(dif)
    
    return lista
