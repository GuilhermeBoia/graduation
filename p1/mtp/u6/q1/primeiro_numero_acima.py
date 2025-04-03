# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Primeiro número acima da médiai


def primeiro_numero_acima(numeros):
    soma = 0
    for num in numeros:
        soma += num

    media = soma / len(numeros)

    tem = False
    for i in range(len(numeros)):
        if numeros[i] > media:
            posicao = i + 1
            tem = True
            break

    if tem:
        return posicao
    else:
        return -1
