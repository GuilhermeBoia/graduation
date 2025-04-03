# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Scroll

def scroll(m):
    for i in range(1, len(m)):
        for j in range(len(m[0])):
            m[i - 1][j] = m[i][j]

    n = len(m) - 1
    for k in range(len(m[n])):
        m[n][k] = 0
