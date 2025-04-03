# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Afinidade Musical

def tem_afinidade(l1, l2):
    count = 0
    for e_l1 in l1:
        for e_l2 in l2:
            if e_l1 == e_l2:
                count += 1

    if count >= 3:
        return True
    else:
        return False
