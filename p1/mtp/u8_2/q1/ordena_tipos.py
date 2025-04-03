# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Ordena Tipos

def ordena_tipos(elementos):
    for j in range(len(elementos)):
        for i in range(len(elementos) -1 -j, 0, -1):
            if elementos[i].isdigit() and not elementos[i-1].isdigit():
                elementos[i], elementos[i-1] = elementos[i-1], elementos[i]

        for k in range(len(elementos)):
            for l in range(len(elementos) - 1 -k, 0, -1):
                if elementos[l].isalpha() and not elementos[l-1].isdigit():
                    elementos[l], elementos[l-1] = elementos[l-1], elementos[l]

    return elementos
