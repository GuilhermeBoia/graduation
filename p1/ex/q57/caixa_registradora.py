# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Caixa Registradora

def caixa_registradora(lista, meta):
    comissao = 0
    soma = 0
    for e in lista:
        soma += e
        if e < 1000:
            comissao += e * 0.05
        elif e < 5000:
            comissao += e * 0.1
        else:
            comissao += e * 0.15
    if soma - comissao >= meta:
        situacao = "Lucro"
    else:
        situacao = "Prejuízo"
    if soma < meta:
        situacao = "Prejuízo"

    saida = []
    saida.append(soma)
    saida.append(comissao)
    saida.append(situacao)
    return saida
