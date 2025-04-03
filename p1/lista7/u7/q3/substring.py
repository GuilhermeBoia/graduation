# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Substring

def is_substring(str1, str2):
    temp = ""
    definitiva = ""
    for l1 in str1:
        for l2 in str2:
            if l1 == l2:
                temp += l2
            if len(temp) > len(definitiva):
                definitiva = temp
                if definitiva == str2:
                    return True

    if definitiva != str2:
        return False

assert is_substring('boiadai','oi')
assert not is_substring('casorio', 'casa')
