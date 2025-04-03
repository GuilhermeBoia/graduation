# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Transposta

def transposta(M):
    T = []
    lista = []
    for i in range(len(M[0])):
        for j in range(len(M)):
            e = M[j][i]
            lista.append(e)
            if j == len(M) - 1:
                T.append(lista)
                lista = []

    return T
