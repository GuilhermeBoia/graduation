linha = input()

copas, espadas, ouro, paus = [], [], [], []

erro_c, erro_e, erro_o, erro_p = False, False, False, False

for i in range(0, len(linha)-2, 3):
    naipe = linha[i+2]

    carta = int(linha[i+1])
    if int(linha[i]) == 1:
        carta += 10

    if naipe == "C":
        if carta in copas:
            erro_c = True
        else:
            copas.append(carta)

    elif naipe == "E":
        if carta in espadas:
            erro_e = True
        else:
            espadas.append(carta)

    elif naipe == "U":
        if carta in ouro:
            erro_o = True
        else:
            ouro.append(carta)

    elif naipe == "P":
        if carta in paus:
            erro_p = True
        else:
            paus.append(carta)

if not erro_c:
    print(13 - len(copas))
else:
    print("erro")

if not erro_e:
    print(13 - len(espadas))
else:
    print("erro")

if not erro_o:
    print(13 - len(ouro))
else:
    print("erro")

if not erro_p:
    print(13 - len(paus))
else:
    print("erro")