linha = input().split(",")

valores = [int(v) for v in linha]

print("Produção Média Trimestral\n")

def media_trimestre(lista, intervalo):
    soma = 0
    for i in range(0, len(lista), intervalo):
        media = (lista[i] + lista[i+1] + lista[i+2]) / intervalo
        return media



numero = 1
trimestre = []
for i in range(0, len(valores), 3):
    media = (valores[i] + valores[i+1] + valores[i+2]) / 3
    trimestre.append(media)
    print(f"trimestre {numero}: {media:.1f}")
    numero += 1

print("\ntrimestre(s) em alta:")

count = 0
for j in range(len(trimestre) - 1):
    if trimestre[j] < trimestre [j+1]:
        print(j+2)
        count += 1

if count == 0:
    print("nenhum")
