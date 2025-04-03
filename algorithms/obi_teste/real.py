linha = input().split()

n, m = int(linha[0]), int(linha[1])
conta = 0

pessoas = [int(e) for e in input().split()]
compareceu = [int(e) for e in input().split()]

filhos = []
netos = []
bisnetos = []
tataranetos = []


for i in range(n):
    if pessoas[i] == 0:
        filhos.append(i+1)
        conta += 1

for e in filhos:
    for i in range(n):
        if e == pessoas[i]:
            netos.append(i+1)
            conta += 1

for e in netos:
    for i in range(n):
        if e == pessoas[i]:
            bisnetos.append(i+1)
            conta += 1

conta_filhos = 0
conta_netos = 0
conta_bisnetos = 0

for e in filhos:
    if e in compareceu:
        conta_filhos += 1

for e in netos:
    if e in compareceu:
        conta_netos += 1

for e in bisnetos:
    if e in compareceu:
        conta_bisnetos += 1

percentual_filhos = conta_filhos / len(filhos) * 100
percentual_netos = conta_netos / len(netos) * 100
percentual_bisnetos = conta_bisnetos / len(bisnetos) * 100


print(f"{percentual_filhos:.2f}", end=" ")
print(f"{percentual_netos:.2f}", end=" ")
print(f"{percentual_bisnetos:.2f}", end=" ")
print("0.00")