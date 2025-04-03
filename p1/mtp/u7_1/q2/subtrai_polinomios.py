# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Subtrai polinomios

def subtrai_polinomios(p1, p2):
    maior = p1
    menor = p2
    if len(p2) > len(p1):
        maior = p2
        menor = p1
    
    resultado = []
    i = 0
    while i < len(maior):
        if i < len(menor):
            diferenca = p1[i] - p2[i]
            resultado.append(diferenca)
        else:
            if maior == p1:
                resultado.append(maior[i])
            else:
                n = -1 * maior[i]
                resultado.append(n)
        i += 1

    for j in range(len(resultado) - 1, -1, -1):
        if resultado[j] == 0:
            resultado.pop(j)
        else: break

    if len(resultado) == 0:
        resultado.append(0)

    return resultado
