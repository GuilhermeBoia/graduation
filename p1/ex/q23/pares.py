# Guilherme Dantas Boia de Albuquerque
# Pares de Múltiplos

# Solicitar uma sequência de números em uma linha e separá-los. E solicitar o fator
valores = input().split()
fator = int(input())

# Tamanho da lista para poder usar no range.
tamanho = (len(valores))
# Criar uma lista vazia para usar no for (aqui serãp adicionados os numeros dados como inteiros)
numeros = []

# Transformar todos os valores pedidos em inteiros e adicionar um por um na lista criada.
for i in range(tamanho):
    num = int(valores[i])
    numeros.append(num)

# Criar uma variável que seja a soma da quantidade de pares de múltiplos
pares = 0
# Criar uma lista vazia para usar no for (aqui serão adicionado os pares de múltiplos)
lista = []

# Pegar a lista de números inteiros, analisar quais são pares de múltiplos vizinhos e adiconá-los em outra lista.
for j in range(tamanho - 1):
    if numeros[j] / numeros [j+1] == fator or numeros[j] / numeros[j+1] == 1 / fator:
        pares += 1
        lista.append(numeros[j])
        lista.append(numeros[j+1])

#Imprimir a soma do número de pares somados.
print(f"{pares} par(es)")

# Tamanho da lista para poder usar no range.
tamanho2 = len(lista)

# Imprimir os pares em uma linha, de acordo com a lista os pares.
for k in range(0, tamanho2, 2):
    print(lista[k], lista[k+1])

