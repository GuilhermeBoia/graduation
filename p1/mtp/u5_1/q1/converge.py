precisao = float(input())

contador = 1
divisor = 1
soma = 0

print(f"{soma:.14f}")

while True:
    soma += 1 / divisor
    contador += 1
    divisor += contador

    print(f"{soma:.14f}")
    if round(2 - soma, 14) <= round(precisao, 14): break
