# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Scroll

def scroll(m):
    for i in range(1, len(m)):
        m[i-1] = m[i]

    lista = []
    for j in range(len(m[0])):
        lista.append(0)

    m[len(m) - 1] = lista
