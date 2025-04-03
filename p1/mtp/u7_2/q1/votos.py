def conta(elemento, lista):
    count = 0
    for e in lista:
        if e == elemento:
            count += 1

    return count

def meu_in(elemento, lista):
    tem = False
    for e in lista:
        if e == elemento:
            tem = True

    return tem

def filtra_votos_validos(votos, candidatos):
    posicao = []
    for e in candidatos:
        for i in range(len(votos)):
            if e != votos[i]:
                posicao.append(i)

    remover = []
    for p in posicao:
        if conta(p, posicao) >= len(candidatos):
            if not meu_in(p, remover):
                remover.append(p)

    for j in range(len(remover)-1, -1, -1):
        votos.pop(remover[j])


votos = [1,1,1,1,12,1,2,1,2,2,2]
candidatos = [1,2]
filtra_votos_validos(votos,candidatos)
assert votos == [1,1,1,1,1,2,1,2,2,2]

votos = [1,2]
candidatos = [1]
filtra_votos_validos(votos,candidatos)
assert votos == [1]

votos = [1,1,2,2,12,3,3,3,4]
candidatos = [1,2,12,4]
filtra_votos_validos(votos,candidatos)
assert votos == [1,1,2,2,12,4]
