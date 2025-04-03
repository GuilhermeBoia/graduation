
linhas = []
linhas2 = []
sequencias = []
i = 1
while True:
    linha = input()
    if linha == "fim": break
    linhas2.append(linhas)
    separado = linha.split()
    inteiros = [int(v) for v in separado]
    #sequencias.append(inteiros)
    soma = 0
    for e in inteiros:
        soma += e

    if soma == len(inteiros):
        linhas.append(linha)
        linhas.append(i)

for j in range(0, len(linhas) - 1, 2):
    print(f"Seq {linhas[j+1]}: {linhas[j]}")

