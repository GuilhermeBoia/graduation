def calcula_digitos_verificacao(cpf):
    digitos = [int(n) for n in cpf]
    fator = 10
    soma = 0
    for e in digitos:
        soma += e * fator
        fator -= 1

    digito_1 = (soma * 10) % 11
    if digito_1 == 10:
        digito_1 = 0
    verificado = str(digito_1)

    soma_2 = digito_1 * 2
    fator_2 = 11
    for el in digitos:
        soma_2 += el * fator_2
        fator_2 -= 1

    digito_2 = (soma_2 * 10) % 11
    if digito_2 == 10:
        digito_2 = 0
    verificado += str(digito_2)

    return verificado
