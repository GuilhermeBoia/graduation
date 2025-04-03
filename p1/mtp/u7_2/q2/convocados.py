def atualiza_convocados(lista, novo_jogador):
    lista.append(novo_jogador)

    for i in range(len(lista)):
        for j in range(len(lista) -1 -i):
            if lista[j][1] > lista[j+1][1]:
                lista[j], lista[j+1] = lista[j+1], lista[j]

            if lista[j][1] == lista[j+1][1]:
                if lista[j][0][0] > lista[j+1][0][0]:
                    lista[j], lista[j+1] = lista[j+1], lista[j]


l = [('Casemiro', 0), ('Marquinhos', 0), ('Fred', 1), ('Neymar', 2)]

assert atualiza_convocados(l, ('Gabigol', 2)) == None
assert l == [('Casemiro', 0), ('Marquinhos', 0), ('Fred', 1), ('Gabigol', 2), ('Neymar', 2)]
