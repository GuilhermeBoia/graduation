# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Controle de temperatura

soma = 0
normal = 0
while True:
    temp = float(input())
    if 10.0 <= temp <= 30.0:
        soma += temp
        normal += 1
    else:
        print(f"Temperatura inadequada! {temp:.2f}.")
        break

if soma == 0:
    media = 0
else:
    media = soma / normal
print(f"{normal} medições lidas dentro do padrão.")
print(f"Média = {media:.2f}.")
