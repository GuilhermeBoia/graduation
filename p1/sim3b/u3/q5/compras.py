# Dia de compras no shopping

movimento = input().split()

compras = int(movimento[1])
dia = movimento[0]

if dia == "sábado" or dia == "domingo":
    if compras < 20:
        print("fraco")
    else:
        print("normal")
else:
    if compras < 20:
        print("fraco")
    elif compras <= 30:
        print("normal")
    else:
        print("atípico")
