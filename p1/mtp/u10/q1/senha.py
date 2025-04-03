# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Registro de Senhas

def meu_in(elemento, lista):
    for e in lista:
        if e == elemento:
            return True
    return False

def senha(cadastro, usuario):
    chaves = cadastro.keys()
    for e in chaves:
        lista = cadastro[e]
        if meu_in(usuario, lista):
            return e

    return -1
