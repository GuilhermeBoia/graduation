# Guilherme Dantas Boia de Albuquerque
# Média Trimestral de uma empresa

# Receber todos os valores em uma so linha e serpará-los por ","
valores = input().split(",")

# Transformá-los em inteiro e adicionar na lista vazia
producao = []
for i in range(len(valores)):
    numeros = int(valores[i])
    producao.append(numeros)

# Separar os valores de 3 meses consecutivos, fazer uma média do trimestre e adicionar na lista vazia
trimestres = []
for j in range(0, len(producao), 3):
    media = (producao[j] + producao[j+1] + producao[j+2]) / 3
    trimestres.append(media)


# Printar o relátorio médio da produção em cada trimestre
print("Produção Média Trimestral\n")
for k in range(len(trimestres)):
    print(f"trimestre {k+1}: {trimestres[k]:.1f}")


# Analisar se o trimestre consecutivo é maior que o anterior. Se nenhum deles for, print "nenhum"
print("\ntrimestre(s) em alta:")
count = 0
for y in range(3):
    if trimestres[y] < trimestres[y+1]:
        print(y+2)
        count += 1 

if count == 0:
    print("nenhum")
