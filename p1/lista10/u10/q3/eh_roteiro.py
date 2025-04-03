#roteiro = "Campina Grande/Recife/Rio de Janeiro"
#cidades = roteiro.split('/')
#codigos = [iata[e] for e in cidades]

def meu_in(elemento, lista):
    for e in lista:
        if e == elemento:
            return True
    return False

def eh_roteiro(iata, voos, roteiro):
    codigos = [iata[e] for e in roteiro.split('/')]

    for i in range(len(codigos) - 1):
        destinos = voos[codigos[i]]
        if meu_in(codigos[i+1], destinos):
            eh_roteiro = True
        else:
            eh_roteiro = False
            break

    return eh_roteiro
