# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Unidade de Medida

si = {"km": 1000, "hm": 100, "dam": 10,
      "m": 1,
      "dm": 0.1, "cm": 0.01, "mm": 0.001}

somas = []
while True:
    medidas = input().split()
    if int(medidas[0]) == 0 and int(medidas[2]) == 0: break

    soma = 0
    for i in range(0, 4, 2):
        fator = si[medidas[i+1]]
        soma += fator * int(medidas[i])
    somas.append(soma)

for e in somas:
    print(f"{e:.2f} m")
