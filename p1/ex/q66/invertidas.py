# Guilherme Dantas Boia de Albuquerque
# Prog1 | 2022.1
# Palavra Invertida

def coincide(str1, str2):
    count = 0
    for i in range(len(str1)):
        if str1[i] == str2[i]:
            count += 1
    return count


palavra = input()

inversa = ""
for i in range(len(palavra) -1, -1, -1):
    inversa += palavra[i]

igual = coincide(palavra, inversa)

print(f"A palavra {palavra} contém {igual} caractere(s) coincidente(s) com a sua inversa.")
