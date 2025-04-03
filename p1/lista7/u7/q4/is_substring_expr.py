# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Expressão regular

def is_substring_expr(str1, str2):
    lista = str2.split("*")
    str2_1 = lista[0]
    str2_2 = lista[1]

    substring = True
    for i in range(len(str2_1)):
        if str2_1[i] != str1[i]:
            substring = False

    if len(str2) - len(str1) == 1:
        substring = False

    diferenca = (len(str1) - len(str2)) + len(str2_1) + 1
    for j in range(len(str2_2) -1, -1, -1):
        if str2_2[j] != str1[j + diferenca]:
            substring = False

    return substring
