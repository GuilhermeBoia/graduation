# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Matriz Menor

def matriz_menor(m1, m2):
    nova_matriz = []
    lista_temp = []

    for i in range(len(m1)):
        for j in range(len(m1[0])):
            if m1[i][j] < m2[i][j]:
                menor = m1[i][j]
            else:
                menor = m2[i][j]

            lista_temp.append(menor)

            if j == len(m1[0]) - 1:
                nova_matriz.append(lista_temp)
                lista_temp = []

    return nova_matriz
