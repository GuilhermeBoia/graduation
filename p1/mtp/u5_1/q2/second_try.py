posicao = []
sequencia = []
i = 0

while True:
    entrada = input()
    if entrada == "fim": break
    inteiros = [int(valor) for valor in entrada.split()]


    soma = 0
    for numero in inteiros:
        soma += numero

    i += 1
    if soma == len(inteiros):
        sequencia.append(entrada)
        posicao.append(i)

for j in range(len(sequencia)):
    print(f"Seq {posicao[j]}: {sequencia[j]}")
