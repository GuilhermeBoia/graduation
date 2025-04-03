# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Hoteis

empresas = []
while True:
    linha = input()
    if linha == "---": break

    valores = linha.split(",")

    for i in range(3):
        valores[i] = float(valores[i])
    empresas.append(valores)

while True:
    linha = input()
    if linha == "fim": break

    hotel = empresas[0][3]
    if linha == "valor":
        termo1 = empresas[0][0]
        for j in range(len(empresas)):
            if empresas[j][0] < termo1:
                termo1 = empresas[j][0]
                hotel = empresas[j][3]

    if linha == "tamanho":
        termo1 = empresas[0][1]
        for j in range(len(empresas)):
            if empresas[j][1] > termo1:
                termo1 = empresas[j][1]
                hotel = empresas[j][3]

    if linha == "conforto":
        termo1 = empresas[0][2]
        for j in range(len(empresas)):
            if empresas[j][2] > termo1:
                termo1 = empresas[j][2]
                hotel = empresas[j][3]
    
    print(hotel)
