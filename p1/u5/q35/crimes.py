# Guilherme Dantas Boia de Albuquerque
# Média de criminalidade

ocorrencias = float(input())
limite = ocorrencias / 2

maiores = []

while True:
    entrada = input()
    if entrada == "fim": break

    numeros = [int(num) for num in entrada.split()]
    soma = 0
    for num in numeros:
        soma += num

    media = soma / len(numeros)
    if media < limite: break
    if media > ocorrencias:
        maiores.append(entrada)

for elemento in maiores:
    print(elemento)
