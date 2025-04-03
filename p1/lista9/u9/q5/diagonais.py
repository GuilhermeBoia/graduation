# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Diagonais

def diagonais(m):
    i = 0
    j = 0
    principal = []
    while i <= len(m) - 1:
        e = m[i][j]
        principal.append(e)
        i += 1
        j += 1

    i = 0
    j = len(m) - 1
    secundaria = []
    while i + j == len(m) - 1 and j >= 0:
        e = m[i][j]
        secundaria.append(e)
        i += 1
        j -= 1

    return [principal, secundaria]
