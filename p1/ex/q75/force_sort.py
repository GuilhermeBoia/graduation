# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Force Sort

def force_sort(seq):
    compara = [n for n in seq]
    
    for i in range(1, len(seq)):
        if seq[i-1] > seq[i]:
            seq[i] = seq[i-1]

    diferenca = []
    for j in range(len(seq)):
        dif = seq[j] - compara[j]
        diferenca.append(dif)

    return diferenca
