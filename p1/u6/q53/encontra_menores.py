# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Encontra menores na lista

def encontra_menores(num, lista):
    for e in lista:
        if e < num:
            return e
    
    return -1
