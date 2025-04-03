def calcular_soma(sequencia):
    soma = 0
    for i in range(len(sequencia)):
        if int(sequencia[i]) == 1:
            soma += 2 ** i
    return soma

sequencia = input().split()
resultado = calcular_soma(sequencia)
print(resultado)