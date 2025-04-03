# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Rotaciona Array

def rotdir(array):
    if len(array) == 2:
        aux = array[0]
        array[0] = array[1]
        array[1] = aux

    if len (array) > 2:
        primeiro = array[0]
        last_idx = len(array) - 1
        array[0] = array[last_idx]

        for i in range(len(array) - 2, 0, -1):
            array[i+1] = array[i]
            if i == 1:
                array[i] = primeiro
