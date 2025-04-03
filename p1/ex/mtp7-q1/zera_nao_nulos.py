# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Zeros Não Nulos

def zera_nao_nulos(m, lin, col):
    i = lin
    j = col 
    while i >= 0:
        if m[i][j] == 0: break
        if m[i][j] != 0:
            m[i][j] = 0
        i -= 1

    i = lin + 1
    j = col
    while i <= len(m) - 1:
        if m[i][j] == 0: break
        if m[i][j] != 0:
            m[i][j] = 0
        i += 1

    i = lin
    j = col - 1
    while j >= 0:
        if m[i][j] == 0: break
        if m[i][j] != 0:
            m[i][j] = 0
        j -= 1

    i = lin
    j = col + 1
    while j <= len(m[0]) - 1:
        if m[i][j] == 0: break
        if m[i][j] != 0:
            m[i][j] = 0
        j += 1
