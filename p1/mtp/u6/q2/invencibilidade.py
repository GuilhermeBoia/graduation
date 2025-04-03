# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Invencibilidade


def invencibilidade(campanha):
    venceu_temp = 0
    venceu_final = 0

    for letra in campanha:
        if letra == "v" or letra == "e":
            venceu_temp += 1
            if venceu_temp > venceu_final:
                venceu_final = venceu_temp

        if letra == "d":
                venceu_temp = 0
    
    return venceu_final


