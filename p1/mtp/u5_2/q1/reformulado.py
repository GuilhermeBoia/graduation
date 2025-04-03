soma = 0
numeros = []
parada = True
while True:
    num = int(input())
    numeros.append(num)
    soma += num

    if soma > 99:
        soma -= num
        break
    if num > 2 * numeros[0]:
        parada = False
        break

if parada:
    media = soma / (len(numeros) - 1)
else:
    media = soma / len(numeros)

print(f"---\nsoma: {soma}\nmédia: {media:.2f}\n")

for i in range(len(numeros)):
    if i == len(numeros) - 1:
        if parada:
            print(f"!!! {numeros[i]}: soma acima de 99")
            break
        else:
            print(f"> {numeros[i]} (acima da média)")
            print(f"!!! {numeros[i]}: maior que dobro do inicial")
            break
            
    if numeros[i] <= media:
        print(f"> {numeros[i]}")
    else:
        print(f"> {numeros[i]} (acima da média)")
