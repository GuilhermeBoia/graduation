# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Senha Segura

def senha_segura(senha):
    if len(senha) >= 4:
        seguro = True
    else:
        seguro = False

    for i in range(len(senha)):
        if (i + 1) % 2 == 0 and int(senha[i]) % 2 != 0:
            seguro = False
        if (i + 1) % 2 != 0 and int(senha[i]) % 2 == 0:
            seguro = False

    if seguro:
        return "Senha segura"
    else:
        return "Senha insegura"
