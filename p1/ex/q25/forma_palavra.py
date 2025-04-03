# Guilherme Dantas Boia de Albuquerque
# Forma palavra

palavra1 = input()
palavra2 = input()
palavra3 = input()
formatacao = ""

for i in range(len(palavra1)):
    if palavra1[i] > palavra2[i] and palavra1[i] > palavra3[i]:
        formatacao = (f"{formatacao}{palavra1[i]}")

    elif palavra2[i] > palavra1[i] and palavra2[i] > palavra3[i]:
        formatacao = (f"{formatacao}{palavra2[i]}")

    else:
        formatacao = (f"{formatacao}{palavra3[i]}")

print(formatacao)
