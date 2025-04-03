# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Quantidade de Usuários

def quantidade_usuarios(cadastro):
    soma = 0
    for e in cadastro.keys():
        if e != 9999:
            soma += len(cadastro[e])

    return soma
